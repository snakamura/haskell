{-# OPTIONS_GHC -fallow-overlapping-instances #-}

module Main where

import qualified Data.ByteString.Lazy as B
import Data.Char (chr, digitToInt)
import Data.Maybe (fromMaybe)
import qualified Database.HDBC as DB
import Network.FastCGI (runFastCGIorCGI)
import Network.NewCGI
import Text.XHtml

import Session


main :: IO ()
main = runFastCGIorCGI $ handleErrors $ withLogin process

process :: UserInfo -> DB.Connection -> CGI CGIResult
process userInfo conn = do
    contentId <- getInput "id"
    content <- liftIO $ getContent conn (userId userInfo) contentId
    case content of
        Just c -> do
            setHeader "Content-Type" $ contentType c
            setHeader "Content-Disposition" (contentDisposition (contentType c) ++ "; name=" ++ contentName c)
            outputFPS $ contentData c
        Nothing -> do
            setHeader "Content-Type" "text/html; charset=utf-8"
--            setStatus 404 "Not Found"
--            output $ renderHtml notFoundPage
            outputNotFound $ "ID: " ++ fromMaybe "" contentId
 where
     contentDisposition contentType
         | beginWith contentType "text/"  = "inline"
         | beginWith contentType "image/" = "inline"
         | otherwise                      = "attachment"
--     beginWith s = and . zipWith (==) s
--     beginWith s = (and.) $ zipWith (==) s
     beginWith = (and.) . zipWith (==)

data Content = Content {
    contentName    :: String,
    contentType    :: String,
    contentData    :: B.ByteString
};

getContent :: DB.Connection -> UserId -> Maybe String -> IO (Maybe Content)
getContent conn userId (Just contentId) =
    do stmt <- DB.prepare conn sql
       DB.execute stmt [DB.toSql contentId, DB.toSql userId]
       r <- DB.fetchRow stmt
       case r of
           Just [n, t, u, c] -> do content <- B.readFile $ "contents/" ++ DB.fromSql c
                                   return $ Just $ Content (DB.fromSql n)
                                                           (DB.fromSql t)
                                                           content
           Nothing           -> return Nothing
 where
     sql = "SELECT name, type, updated, content FROM data WHERE id=? AND userid=?"
getContent _ _ _ = return Nothing

notFoundPage :: Html
notFoundPage =
    head +++
    body << p << "The specified content is not found."
 where
     head = header << (thetitle << "Error" +++
                       meta ! [httpequiv "Content-Type", content "text/html; charset=utf-8"])
