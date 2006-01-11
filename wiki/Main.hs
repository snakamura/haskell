module Main (main)
    where

import           Control.Monad
import           System
import           System.IO     as IO
import qualified System.Time   as Time
import qualified Text.Printf   as Printf
import qualified Text.Regex    as Regex

import qualified CGI
import qualified Config
import qualified FileStore
import qualified HTMLUtil
import qualified Store
import qualified Template


cgi, defaultPage :: String
cgi         = "wiki.cgi"
defaultPage = "FrontPage"

main :: IO ()
main = Config.loadConfig "./config" >>= processMain

processMain :: Config.Config -> IO ()
processMain config = CGI.process $ process store config
    where
        store = FileStore.newFileStore dataDir
        dataDir = Config.getConfig config "dataDir"

process :: Store.Store a => a -> Config.Config -> CGI.HTTPRequest -> IO CGI.HTTPResponse
process store config request = do
    case getMode $ CGI.getParam params "mode" of
         VIEW   -> do b <- Store.existPage store page
                      if b then processView store config page
                           else processEdit store config page
         EDIT   -> processEdit store config page
         UPDATE -> do if body /= "" then do Store.updatePage store page body
                                            processUpdate store config page
                                    else do Store.removePage store page
                                            processUpdate store config defaultPage
         LIST   -> processList store config
    where
        params = CGI.requestParams request
        page = if page' == "" then defaultPage else page'
        page' = CGI.getParam params "page"
        body = CGI.getParam params "body"

processView :: Store.Store a => a -> Config.Config -> String -> IO CGI.HTTPResponse
processView store config page = do
    template <- loadConfigTemplate config "view.html"
    body <- Store.getPage store page
    formattedBody <- formatBody store body
    return $ CGI.HTTPResponse header $ Template.evalTemplate template $ variables body formattedBody
    where
        header = [ ("Content-Type", getContentType config) ]
        variables body formattedBody = [ ("cgi",           cgi          ),
                                         ("page",          page         ),
                                         ("body",          body         ),
                                         ("formattedBody", formattedBody) ]

processEdit :: Store.Store a => a -> Config.Config -> String -> IO CGI.HTTPResponse
processEdit store config page = do
    template <- loadConfigTemplate config "edit.html"
    body <- Store.getPage store page
    return $ CGI.HTTPResponse header $ Template.evalTemplate template $ variables body
    where
        header = [ ("Content-Type", getContentType config) ]
        variables body = [ ("cgi",  cgi ),
                           ("page", page),
                           ("body", body) ]

processUpdate :: Store.Store a => a -> Config.Config -> String -> IO CGI.HTTPResponse
processUpdate store config page = do
    template <- loadConfigTemplate config "update.html"
    return $ CGI.HTTPResponse header $ Template.evalTemplate template variables
    where
        header = [ ("Content-Type", getContentType config),
                   ("Location",     thisURL)               ]
        variables = [ ("cgi",  cgi    ),
                      ("page", page   ),
                      ("url",  thisURL) ]
        thisURL = baseURL ++ cgi ++ "?page=" ++ HTMLUtil.encodeURLComponent page
        baseURL = Config.getConfig config "baseURL"

processList :: Store.Store a => a -> Config.Config -> IO CGI.HTTPResponse
processList store config = do
    template <- loadConfigTemplate config "list.html"
    list <- Store.listPages store
    content <- formatList list
    return $ CGI.HTTPResponse header $ Template.evalTemplate template $ variables content
    where
        header = [ ("Content-Type", getContentType config) ]
        variables content = [ ("cgi",      cgi    ),
                              ("content",  content) ]
        formatList :: [Store.PageMetadata] -> IO String
        formatList list = do f <- mapM formatMetadata list
                             return $ "<ul>\n" ++ concat f ++ "</ul>\n"
        formatMetadata :: Store.PageMetadata -> IO String
        formatMetadata metadata = do
            c <- Time.toCalendarTime $ Store.lastModified metadata
            return $ "<li>" ++ formatDate c ++ " : " ++ formatPage pageName pageName ++ "</li>\n"
            where
                pageName = Store.name $ metadata
        formatDate :: Time.CalendarTime -> String
        formatDate c = Printf.printf "%04d/%02d/%02d %02d:%02d:%02d"
                           (Time.ctYear c) ((fromEnum $ Time.ctMonth c) + 1) (Time.ctDay c)
                           (Time.ctHour c) (Time.ctMin c) (Time.ctSec c)

getContentType :: Config.Config -> String
getContentType config = "text/html; charset=" ++ charset
    where
        charset = Config.getConfig config "charset"


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
        f ""   = return "</p>\n<p>"
        f line = formatInline store line >>= return . (++ "\n")
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
        regex = Regex.mkRegex "\\b(([A-Z]([A-Za-z])+){2,})\\b|((https?|ftp)://[A-Za-z0-9_=/.?&~+-]+)"
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

loadConfigTemplate :: Config.Config -> String -> IO Template.Template
loadConfigTemplate config name = Template.loadTemplate $ templateDir ++ name
    where
        templateDir = Config.getConfig config "templateDir"
