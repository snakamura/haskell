module Main where

import Control.Monad
import Data.Char
import System
import System.Environment
import System.IO
import System.IO.Error
import System.Time
import Text.Printf
import Text.Regex

import FileStore
import HTMLUtil
import Store
import Template

dataDir, templateDir, cgi, cgiURL, defaultPage, charset :: String
dataDir = "/home/snakamura/haskell/wiki/data/"
templateDir = "/home/snakamura/haskell/wiki/template/"
cgi = "wiki.cgi"
cgiURL = "http://home.snak.org/~snakamura/" ++ cgi
defaultPage = "FrontPage"
charset = "euc-jp"

main :: IO ()
main = do query <- getQuery
          content <- hGetContents stdin
          process (newFileStore dataDir) (parseParams query ++ parseParams content)
    where
        getQuery :: IO String
        getQuery = catch (getEnv "QUERY_STRING")
                         (\ e -> if isDoesNotExistError e
                                    then return ""
                                    else ioError e)

process :: Store a => a -> Params -> IO ()
process store params = do
    case getMode $ getParam params "mode" of
         VIEW   -> do b <- existPage store page
                      if b then printViewHtml store page
                           else printEditHtml store page
         EDIT   -> printEditHtml store page
         UPDATE -> do if body /= "" then do updatePage store page body
                                            printUpdateHtml store page
                                    else do removePage store page
                                            printUpdateHtml store defaultPage
         LIST -> printListHtml store
    where
        page = if page' == "" then defaultPage else page'
        page' = getParam params "page"
        body = getParam params "body"

printViewHtml :: Store a => a -> String -> IO ()
printViewHtml store page = do
    template <- loadTemplate $ templateDir ++ "view.html"
    body <- getPage store page
    formattedBody <- formatBody store body
    printContentType
    printLine ""
    putStrLn $ evalTemplate template [ ("cgi",           cgi          ),
                                       ("page",          page         ),
                                       ("body",          body         ),
                                       ("formattedBody", formattedBody) ]

printEditHtml :: Store a => a -> String -> IO ()
printEditHtml store page = do
    template <- loadTemplate $ templateDir ++ "edit.html"
    body <- getPage store page
    printContentType
    printLine ""
    putStrLn $ evalTemplate template [ ("cgi",  cgi ),
                                       ("page", page),
                                       ("body", body) ]

printUpdateHtml :: Store a => a -> String -> IO ()
printUpdateHtml store page = do
    template <- loadTemplate $ templateDir ++ "update.html"
    printContentType
    printLine $ "Location: " ++ thisURL
    printLine ""
    putStrLn $ evalTemplate template [ ("cgi",  cgi    ),
                                       ("page", page   ),
                                       ("url",  thisURL) ]
    where
        thisURL = cgiURL ++ "?page=" ++ encodeURLComponent page

printListHtml :: Store a => a -> IO ()
printListHtml store = do
    template <- loadTemplate $ templateDir ++ "list.html"
    list <- listPages store
    content <- formatList list
    printContentType
    printLine ""
    putStrLn $ evalTemplate template [ ("cgi",      cgi    ),
                                       ("content",  content) ]
    where
        formatList :: [PageMetadata] -> IO String
        formatList list = do f <- mapM formatMetadata list
                             return $ "<ul>\r\n" ++ concat f ++ "</ul>\r\n"
        formatMetadata :: PageMetadata -> IO String
        formatMetadata (PageMetadata name time) = do
            c <- toCalendarTime time
            return $ "<li>" ++ formatDate c ++ " : " ++ formatPage name name ++ "</li>\r\n"
        formatDate :: CalendarTime -> String
        formatDate c = printf "%04d/%02d/%02d %02d:%02d:%02d"
                           (ctYear c) ((fromEnum $ ctMonth c) + 1) (ctDay c)
                           (ctHour c) (ctMin c) (ctSec c)

printContentType :: IO ()
printContentType = printLine $ "Content-Type: text/html; charset=" ++ charset

printLine :: String -> IO ()
printLine s = do putStr s
                 putStr "\r\n"

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
         Nothing -> return $ escapeHtml s
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


data Mode = VIEW
          | EDIT
          | UPDATE
          | LIST

getMode :: String -> Mode
getMode mode | mode == "edit"   = EDIT
             | mode == "update" = UPDATE
             | mode == "list"   = LIST
             | otherwise        = VIEW


type Param = (String, String)
type Params = [Param]

getParam :: Params -> String -> String
getParam params name = case lookup name params of
                            Just v  -> v
                            Nothing -> ""

parseParams :: String -> Params
parseParams = map parseParam . paramList
    where
        paramList :: String -> [String]
        paramList "" = []
        paramList s = case splitString '&' s of
                           (_, [])    -> [s]
                           (xs, ys) -> xs:(paramList ys)

parseParam :: String -> Param
parseParam = decodeValue . splitString '='
    where
        decodeValue :: Param -> Param
        decodeValue (name, value) = (name, decode value)
        decode :: String -> String
        decode "" = ""
        decode ('%':r@(c1:c2:s))
            | isHexDigit c1 && isHexDigit c2 = (decodeChar c1 c2):(decode s)
            | otherwise                      = '%':(decode r)
        decode (c:s) | c == '+'  = ' ':(decode s)
                     | otherwise = c:(decode s)
        decodeChar :: Char -> Char -> Char
        decodeChar c1 c2 = chr ((digitToInt c1)*16 + digitToInt c2)


splitString :: Char -> String -> (String, String)
splitString c s = case break (c ==) s of
                       (_, [])    -> (s, [])
                       (xs, _:ys) -> (xs, ys)

encodeURLComponent :: String -> String
encodeURLComponent = concatMap encodeURLComponentChar
    where
        encodeURLComponentChar :: Char -> String
        encodeURLComponentChar c
            | 'A' <= c && c <= 'Z' = [c]
            | 'a' <= c && c <= 'z' = [c]
            | '0' <= c && c <= '9' = [c]
            | c == '_'             = [c]
            | otherwise            = '%':(intToDigit $ n `div` 16):(intToDigit $ n `mod` 16):[]
            where
                n = ord c
