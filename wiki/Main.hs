module Main (main)
    where

import Control.Monad
import System
import System.IO
import System.Time
import Text.Printf
import Text.Regex

import CGI
import Config
import FileStore
import HTMLUtil
import Store
import Template

cgi, defaultPage :: String
cgi         = "wiki.cgi"
defaultPage = "FrontPage"

main :: IO ()
main = loadConfig "./config" >>= processMain

processMain :: Config -> IO ()
processMain config = requestParams >>= process (newFileStore dataDir) config
    where
        requestParams :: IO Params
        requestParams = do method <- getRequestMethod
                           case method of
                                GET  -> getQuery >>= return . parseParams
                                POST -> hGetContents stdin >>= return . parseParams
        dataDir = getConfig config "dataDir"

process :: Store a => a -> Config -> Params -> IO ()
process store config params = do
    case getMode $ getParam params "mode" of
         VIEW   -> do b <- existPage store page
                      if b then printViewHtml store config page
                           else printEditHtml store config page
         EDIT   -> printEditHtml store config page
         UPDATE -> do if body /= "" then do updatePage store page body
                                            printUpdateHtml store config page
                                    else do removePage store page
                                            printUpdateHtml store config defaultPage
         LIST -> printListHtml store config
    where
        page = if page' == "" then defaultPage else page'
        page' = getParam params "page"
        body = getParam params "body"

printViewHtml :: Store a => a -> Config -> String -> IO ()
printViewHtml store config page = do
    template <- loadConfigTemplate config "view.html"
    body <- getPage store page
    formattedBody <- formatBody store body
    printContentType config
    printLine ""
    putStrLn $ evalTemplate template [ ("cgi",           cgi          ),
                                       ("page",          page         ),
                                       ("body",          body         ),
                                       ("formattedBody", formattedBody) ]

printEditHtml :: Store a => a -> Config -> String -> IO ()
printEditHtml store config page = do
    template <- loadConfigTemplate config "edit.html"
    body <- getPage store page
    printContentType config
    printLine ""
    putStrLn $ evalTemplate template [ ("cgi",  cgi ),
                                       ("page", page),
                                       ("body", body) ]

printUpdateHtml :: Store a => a -> Config -> String -> IO ()
printUpdateHtml store config page = do
    template <- loadConfigTemplate config "update.html"
    printContentType config
    printLine $ "Location: " ++ thisURL
    printLine ""
    putStrLn $ evalTemplate template [ ("cgi",  cgi    ),
                                       ("page", page   ),
                                       ("url",  thisURL) ]
    where
        thisURL = getConfig config "baseURL" ++ cgi ++ "?page=" ++ encodeURLComponent page

printListHtml :: Store a => a -> Config -> IO ()
printListHtml store config = do
    template <- loadConfigTemplate config "list.html"
    list <- listPages store
    content <- formatList list
    printContentType config
    printLine ""
    putStrLn $ evalTemplate template [ ("cgi",      cgi    ),
                                       ("content",  content) ]
    where
        formatList :: [PageMetadata] -> IO String
        formatList list = do f <- mapM formatMetadata list
                             return $ "<ul>\r\n" ++ concat f ++ "</ul>\r\n"
        formatMetadata :: PageMetadata -> IO String
        formatMetadata metadata = do
            c <- toCalendarTime $ lastModified metadata
            return $ "<li>" ++ formatDate c ++ " : " ++ formatPage pageName pageName ++ "</li>\r\n"
            where
                pageName = name $ metadata
        formatDate :: CalendarTime -> String
        formatDate c = printf "%04d/%02d/%02d %02d:%02d:%02d"
                           (ctYear c) ((fromEnum $ ctMonth c) + 1) (ctDay c)
                           (ctHour c) (ctMin c) (ctSec c)

printContentType :: Config -> IO ()
printContentType config = printLine $ "Content-Type: text/html; charset=" ++ (getConfig config "charset")

printLine :: String -> IO ()
printLine s = do putStr s
                 putStr "\r\n"


data Mode = VIEW
          | EDIT
          | UPDATE
          | LIST

getMode :: String -> Mode
getMode mode | mode == "edit"   = EDIT
             | mode == "update" = UPDATE
             | mode == "list"   = LIST
             | otherwise        = VIEW


formatBody :: Store a => a -> String -> IO String
formatBody store body = do x <- foldM (\ l r -> f r >>= return . (l ++))
                                      [] (lines $ normalizeNewLine body)
                           return $ "<p>" ++ x ++ "</p>"
    where
        f :: String -> IO String
        f ""   = return "</p>\r\n<p>"
        f line = formatInline store line >>= return . (++ "\r\n")
        normalizeNewLine = filter ('\r' /=)

formatInline :: Store a => a -> String -> IO String
formatInline = formatInlineText

formatInlineText :: Store a => a -> String -> IO String
formatInlineText store s =
    case matchRegexAll regex s of
         Just (b, m, a, _:w:_) -> do f <- if w /= "" then formatPage' m
                                                     else formatURL' m
                                     r <- formatInlineText store a
                                     return $ escapeHtml b ++ f ++ r
         Nothing               -> return $ escapeHtml s
    where
        regex = mkRegex "\\b(([A-Z]([A-Za-z])+){2,})\\b|((https?|ftp)://[A-Za-z0-9_=/.?&+-]+)"
        formatPage' :: String -> IO String
        formatPage' page = do e <- existPage store page
                              if e then return $ formatPage page page
                                   else return $ formatPage page "?" ++ page
        formatURL' :: String -> IO String
        formatURL' url = return $ formatURL url url

formatPage :: String -> String -> String
formatPage page content = formatURL (cgi ++ "?page=" ++ page) content

formatURL :: String -> String -> String
formatURL url content = "<a href=\"" ++ url ++ "\">" ++ content ++ "</a>"

loadConfigTemplate :: Config -> String -> IO Template
loadConfigTemplate config name = loadTemplate $ templateDir ++ name
    where
        templateDir = getConfig config "templateDir"
