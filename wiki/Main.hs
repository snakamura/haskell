module Main (main)
    where

import           Control.Monad
import           System
import           System.IO     as IO
import qualified System.Time   as Time
import           Text.Printf
import qualified Text.Regex    as Regex

import qualified CGI
import           Config
import qualified FileStore
import qualified HTMLUtil
import qualified Store
import qualified Template


cgi, defaultPage :: String
cgi         = "wiki.cgi"
defaultPage = "FrontPage"

main :: IO ()
main = loadConfig "./config" >>= processMain

processMain :: Config -> IO ()
processMain config = requestParams >>= process (FileStore.newFileStore dataDir) config
    where
        requestParams :: IO CGI.Params
        requestParams = do method <- CGI.getRequestMethod
                           case method of
                                CGI.GET  -> CGI.getQuery >>= return . CGI.parseParams
                                CGI.POST -> IO.hGetContents IO.stdin >>= return . CGI.parseParams
        dataDir = getConfig config "dataDir"

process :: Store.Store a => a -> Config -> CGI.Params -> IO ()
process store config params = do
    case getMode $ CGI.getParam params "mode" of
         VIEW   -> do b <- Store.existPage store page
                      if b then printViewHtml store config page
                           else printEditHtml store config page
         EDIT   -> printEditHtml store config page
         UPDATE -> do if body /= "" then do Store.updatePage store page body
                                            printUpdateHtml store config page
                                    else do Store.removePage store page
                                            printUpdateHtml store config defaultPage
         LIST -> printListHtml store config
    where
        page = if page' == "" then defaultPage else page'
        page' = CGI.getParam params "page"
        body = CGI.getParam params "body"

printViewHtml :: Store.Store a => a -> Config -> String -> IO ()
printViewHtml store config page = do
    template <- loadConfigTemplate config "view.html"
    body <- Store.getPage store page
    formattedBody <- formatBody store body
    printContentType config
    printLine ""
    putStrLn $ Template.evalTemplate template [ ("cgi",           cgi          ),
                                                ("page",          page         ),
                                                ("body",          body         ),
                                                ("formattedBody", formattedBody) ]

printEditHtml :: Store.Store a => a -> Config -> String -> IO ()
printEditHtml store config page = do
    template <- loadConfigTemplate config "edit.html"
    body <- Store.getPage store page
    printContentType config
    printLine ""
    putStrLn $ Template.evalTemplate template [ ("cgi",  cgi ),
                                                ("page", page),
                                                ("body", body) ]

printUpdateHtml :: Store.Store a => a -> Config -> String -> IO ()
printUpdateHtml store config page = do
    template <- loadConfigTemplate config "update.html"
    printContentType config
    printLine $ "Location: " ++ thisURL
    printLine ""
    putStrLn $ Template.evalTemplate template [ ("cgi",  cgi    ),
                                                ("page", page   ),
                                                ("url",  thisURL) ]
    where
        thisURL = getConfig config "baseURL" ++ cgi ++ "?page=" ++ HTMLUtil.encodeURLComponent page

printListHtml :: Store.Store a => a -> Config -> IO ()
printListHtml store config = do
    template <- loadConfigTemplate config "list.html"
    list <- Store.listPages store
    content <- formatList list
    printContentType config
    printLine ""
    putStrLn $ Template.evalTemplate template [ ("cgi",      cgi    ),
                                                ("content",  content) ]
    where
        formatList :: [Store.PageMetadata] -> IO String
        formatList list = do f <- mapM formatMetadata list
                             return $ "<ul>\r\n" ++ concat f ++ "</ul>\r\n"
        formatMetadata :: Store.PageMetadata -> IO String
        formatMetadata metadata = do
            c <- Time.toCalendarTime $ Store.lastModified metadata
            return $ "<li>" ++ formatDate c ++ " : " ++ formatPage pageName pageName ++ "</li>\r\n"
            where
                pageName = Store.name $ metadata
        formatDate :: Time.CalendarTime -> String
        formatDate c = printf "%04d/%02d/%02d %02d:%02d:%02d"
                           (Time.ctYear c) ((fromEnum $ Time.ctMonth c) + 1) (Time.ctDay c)
                           (Time.ctHour c) (Time.ctMin c) (Time.ctSec c)

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


formatBody :: Store.Store a => a -> String -> IO String
formatBody store body = do x <- foldM (\ l r -> f r >>= return . (l ++))
                                      [] (lines $ normalizeNewLine body)
                           return $ "<p>" ++ x ++ "</p>"
    where
        f :: String -> IO String
        f ""   = return "</p>\r\n<p>"
        f line = formatInline store line >>= return . (++ "\r\n")
        normalizeNewLine = filter ('\r' /=)

formatInline :: Store.Store a => a -> String -> IO String
formatInline = formatInlineText

formatInlineText :: Store.Store a => a -> String -> IO String
formatInlineText store s =
    case Regex.matchRegexAll regex s of
         Just (b, m, a, _:w:_) -> do f <- if w /= "" then formatPage' m
                                                     else formatURL' m
                                     r <- formatInlineText store a
                                     return $ HTMLUtil.escapeHtml b ++ f ++ r
         Nothing               -> return $ HTMLUtil.escapeHtml s
    where
        regex = Regex.mkRegex "\\b(([A-Z]([A-Za-z])+){2,})\\b|((https?|ftp)://[A-Za-z0-9_=/.?&+-]+)"
        formatPage' :: String -> IO String
        formatPage' page = do e <- Store.existPage store page
                              if e then return $ formatPage page page
                                   else return $ formatPage page "?" ++ page
        formatURL' :: String -> IO String
        formatURL' url = return $ formatURL url url

formatPage :: String -> String -> String
formatPage page content = formatURL (cgi ++ "?page=" ++ page) content

formatURL :: String -> String -> String
formatURL url content = "<a href=\"" ++ url ++ "\">" ++ content ++ "</a>"

loadConfigTemplate :: Config -> String -> IO Template.Template
loadConfigTemplate config name = Template.loadTemplate $ templateDir ++ name
    where
        templateDir = getConfig config "templateDir"
