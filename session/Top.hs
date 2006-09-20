{-# OPTIONS_GHC -fallow-overlapping-instances #-}

module Main where

import qualified Database.HDBC as DB
import Network.FastCGI (runFastCGIorCGI)
import Network.NewCGI
import System.Locale (defaultTimeLocale, rfc822DateFormat)
import System.Time (CalendarTime(), formatCalendarTime, toCalendarTime)
import Text.Regex.Lazy
import Text.Regex.PCRE ((=~~))
import Text.XHtml

import DateTime
import Session
import Util


logoutPage  = "logout.cgi"
contentPage = "content.cgi"

main :: IO ()
main = runFastCGIorCGI $ handleErrors $ withLogin process

process :: UserInfo -> DB.Connection -> CGI CGIResult
process userInfo conn = do
    contents <- liftIO $ getData conn $ userId userInfo
    setHeader "Content-Type" "text/html; charset=utf-8"
    output $ renderHtml $ topPage (displayName userInfo) contents

data Content = Content {
    contentId      :: Int,
    contentName    :: String,
    contentType    :: String,
    contentUpdated :: CalendarTime,
    contentComment :: String
};

getData :: DB.Connection -> UserId -> IO [Content]
getData conn userid = do stmt <- DB.prepare conn sql
                         DB.execute stmt [DB.toSql userid]
                         unfoldM fetch stmt
 where
     fetch :: DB.Statement -> IO (Maybe (Content, DB.Statement))
     fetch stmt = do r <- DB.fetchRow stmt
                     case r of
                         Just r  -> do c <- createContent r
                                       return $ Just (c, stmt)
                         Nothing -> return Nothing
      where
          createContent [id, name, contentType, updated, comment] =
              do u <- toCalendarTime $ parseDateTime $ DB.fromSql updated
                 return $ Content (DB.fromSql id)
                                  (DB.fromSql name)
                                  (DB.fromSql contentType)
                                  u
                                  (DB.fromSql comment)
     sql = "SELECT id, name, type, updated, comment FROM data WHERE userid=?"

topPage :: String -> [Content] -> Html
topPage userName contents =
    head +++
    body << (
        p << (
            thespan << ("Welcome, " ++ userName) +++
            thespan << anchor ! [href logoutPage] << "Logout") +++
        table << map format contents +++
        hr +++
        uploadForm +++
        overDiv)
 where
     head = header << (thetitle << "Top" +++
                       meta ! [httpequiv "Content-Type",
                               content "text/html; charset=utf-8"] +++
                       thelink ! [rel "stylesheet",
                                  thetype "text/css",
                                  href "style/top.css",
                                  strAttr "media" "all"] << noHtml +++
                       itag "script" ! [thetype "text/javascript",
                                        src "script/top.js"] +++
                       itag "script" ! [thetype "text/javascript",
                                        src "script/overlib.js"] +++
                       itag "script" ! [thetype "text/javascript",
                                        src "script/prototype.js"])
     uploadForm = form ! [method "post",
                          action "upload.cgi",
                          enctype "multipart/form-data"] << (
                      p << input ! [thetype "file", name "file"] +++
                      p << input ! [thetype "text", name "comment"] +++
                      p << input ! [thetype "submit", value "Upload"])
     overDiv = thediv ! [identifier "overDiv",
                         thestyle "position:absolute; visibility:hidden; z-index:1000"] << noHtml
     format :: Content -> Html
     format c = tr ! [identifier ("item" ++ id),
                      strAttr "onmouseover" ("return showTip(this, " ++ id ++ ", '" ++ escapeSingleQuote(contentType c) ++ "')"),
                      strAttr "onmouseout" "return hideTip(this)"] << (
                    td << formatCalendarTime defaultTimeLocale rfc822DateFormat (contentUpdated c) +++
                    td << anchor ! [href (contentPage ++ "?id=" ++ id)] << contentName c +++
                    td << contentComment c)
      where
          id = show $ contentId c
          escapeSingleQuote = concatMap (\c -> if c == '\'' then "\\\'" else [c])

