module Main
    where

import Char
import Directory
import IO
import List
import Monad
import System
import System.Environment
import System.Time
import Text.Regex

dataDir, cgi, cgiURL, defaultPage, charset :: String
dataDir = "/home/snakamura/haskell/wiki/data/"
cgi = "wiki.cgi"
cgiURL = "http://home.snak.org/~snakamura/" ++ cgi
defaultPage = "FrontPage"
charset = "euc-jp"

main :: IO ()
main = do query <- getQuery
          content <- hGetContents stdin
          process $ parseParams query ++ parseParams content
    where
        getQuery :: IO String
        getQuery = catch (getEnv "QUERY_STRING")
                         (\ e -> if isDoesNotExistError e
                                    then return ""
                                    else ioError e)

process :: Params -> IO ()
process params = do
    case getMode $ getParam params "mode" of
         VIEW   -> do b <- existData page
                      if b then printViewHtml page
                           else printEditHtml page
         EDIT   -> printEditHtml page
         UPDATE -> do if body /= "" then do updateData page body
                                            printUpdateHtml page
                                    else do removeData page
                                            printUpdateHtml defaultPage
         LIST -> printListHtml
    where
        page = if page' == "" then defaultPage else page'
        page' = getParam params "page"
        body = getParam params "body"

printViewHtml :: String -> IO ()
printViewHtml page = do
    body <- getData page
    formattedBody <- formatBody body
    printContentType
    printLine ""
    printLine "<html>"
    printLine "<head>"
    printLine $ "<title>" ++ escapeHtml page ++ "</title>"
    printLine "</head>"
    printLine "<body>"
    printNavigator page
    printLine $ "<h1>" ++ escapeHtml page ++ "</h1>"
    printLine formattedBody
    printLine "</body>"
    printLine "</html>"

printEditHtml :: String -> IO ()
printEditHtml page = do
    body <- getData page
    printContentType
    printLine ""
    printLine "<html>"
    printLine "<head>"
    printLine $ "<title>" ++ escapeHtml page ++ "</title>"
    printLine "</head>"
    printLine "<body>"
    printNavigator page
    printLine $ "<h1>" ++ escapeHtml page ++ "</h1>"
    printLine $ "<form action=\""++ cgi ++ "\" method=\"POST\">"
    putStr "<div><textarea name=\"body\" rows=\"20\" cols=\"80\">"
    putStr $ escapeHtml body
    printLine "</textarea></div>"
    printLine "<div><input type=\"submit\" value=\"Save\"></input></div>"
    printLine $ "<input name=\"page\" type=\"hidden\" value=\"" ++ escapeHtml page ++ "\"></input>"
    printLine "<input name=\"mode\" type=\"hidden\" value=\"update\"></input>"
    printLine "</form>"
    printLine "</body>"
    printLine "</html>"

printUpdateHtml :: String -> IO ()
printUpdateHtml page = do
    printContentType
    printLine $ "Location: " ++ thisURL
    printLine ""
    printLine "<html>"
    printLine "<head>"
    printLine $ "<meta http-equiv=\"refresh\" content=\"0;url=" ++ thisURL ++ "\">"
    printLine $ "<title>" ++ escapeHtml page ++ "</title>"
    printLine "</head>"
    printLine "<body>"
    printLine $ "<p><a href=\"" ++ cgi ++ "?page=" ++ encodeURLComponent page ++ "\">Click here</a></p>"
    printLine "</body>"
    printLine "</html>"
    where
        thisURL = cgiURL ++ "?page=" ++ encodeURLComponent page

printListHtml :: IO ()
printListHtml = do
    list <- listData
    printContentType
    printLine ""
    printLine "<html>"
    printLine "<head>"
    printLine $ "<title>List</title>"
    printLine "</head>"
    printLine "<body>"
    printNavigator ""
    printLine $ "<h1>List</h1>"
    printLine "<ul>"
    mapM_ printIndex list
    printLine "</ul>"
    printLine "</body>"
    printLine "</html>"
    where
        printIndex :: DataMetadata -> IO ()
        printIndex (DM s t) = do c <- toCalendarTime t
                                 printLine $ "<li>" ++ formatDate c ++ " : " ++ formatPage s s ++ "</li>"
        formatDate :: CalendarTime -> String
        formatDate c = (show $ ctYear c) ++ "/" ++
                       (show $ (fromEnum $ ctMonth c) + 1) ++ "/" ++
                       (show $ ctDay c) ++ " " ++
                       (show $ ctHour c) ++ ":" ++
                       (show $ ctMin c) ++ ":" ++
                       (show $ ctSec c)

printContentType :: IO ()
printContentType = printLine $ "Content-Type: text/html; charset=" ++ charset

printNavigator :: String -> IO ()
printNavigator page = do
    printLine "<div>"
    if page /= ""
       then printLine $ "<a href=\"" ++ cgiURL ++ "?mode=edit&page=" ++ encodeURLComponent page ++ "\">Edit</a> | "
       else return ()
    printLine $ "<a href=\"" ++ cgiURL ++ "?page=FrontPage\">Top</a> | "
    printLine $ "<a href=\"" ++ cgiURL ++ "?mode=list\">List</a>"
    printLine "</div>"

printLine :: String -> IO ()
printLine s = do putStr s
                 putStr "\r\n"

formatBody :: String -> IO String
formatBody body = do x <- foldM (\ l r -> f r >>= return . (l ++))
                                [] (lines $ normalizeNewLine body)
                     return $ "<p>" ++ x ++ "</p>"
    where
        f :: String -> IO String
        f ""   = return "</p>\n<p>"
        f line = do l <- formatInline line
                    return $ l ++ "\n"
        normalizeNewLine = filter ('\r' /=)

formatInline :: String -> IO String
formatInline s = case matchRegexAll regex s of
                      Just (b, m, a, _:w:_) -> do f <- if w /= "" then formatPage' m
                                                                  else formatURL' m
                                                  r <- formatInline a
                                                  return $ escapeHtml b ++ f ++ r
                      Nothing -> return $ escapeHtml s
    where
        regex = mkRegex "\\b(([A-Z]([A-Za-z])+){2,})\\b|((https?|ftp)://[A-Za-z0-9_=/.?&+-]+)"
        formatPage' :: String -> IO String
        formatPage' page = do e <- existData page
                              if e then return $ formatPage page page
                                   else return $ formatPage page "?" ++ page
        formatURL' :: String -> IO String
        formatURL' url = return $ formatURL url url

formatPage :: String -> String -> String
formatPage page content = formatURL (cgiURL ++ "?page=" ++ page) content

formatURL :: String -> String -> String
formatURL url content = "<a href=\"" ++ url ++ "\">" ++ content ++ "</a>"

data DataMetadata = DM String ClockTime

existData :: String -> IO Bool
existData = doesFileExist . getDataPath

getData :: String -> IO String
getData page = catch (readFile $ getDataPath page)
                     (\ e -> return "")

updateData :: String -> String -> IO ()
updateData = writeFile . getDataPath

removeData :: String -> IO ()
removeData = removeFile . getDataPath

listData :: IO [DataMetadata]
listData = getDirectoryContents dataDir >>=
           filterM isFile >>=
           return . sort >>=
           mapM pairFileTime
    where
        isFile :: String -> IO Bool
        isFile (c:_)  = return $ c /= '.'
        pairFileTime :: String -> IO DataMetadata
        pairFileTime file = (getModificationTime $ getDataPath file) >>=
                            return . DM file

getDataPath :: String -> FilePath
getDataPath page = dataDir ++ page


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
{-
getParam [] _ = ""
getParam ((n, v):params) name
    | n == name = v
    | otherwise = getParam params name 
-}

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

escapeHtml :: String -> String
--escapeHtml = foldr ((++) . escapeHtmlChar) []
escapeHtml = concatMap escapeHtmlChar
    where
        escapeHtmlChar :: Char -> String
        escapeHtmlChar c | c == '<'  = "&lt;"
                         | c == '>'  = "&gt;"
                         | c == '&'  = "&amp;"
                         | c == '\"' = "&quot;"
                         | otherwise = [c]

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
