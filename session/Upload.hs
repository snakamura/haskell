{-# OPTIONS_GHC -fallow-overlapping-instances #-}

module Main where

import Control.Exception (Exception(IOException), throwIO)
import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import qualified Database.HDBC as DB
import qualified Data.ByteString.Lazy as B
import Network.FastCGI (runFastCGIorCGI)
import Network.NewCGI
import SHA1 (sha1)
import System.Random (randomIO)
import System.Time (getClockTime)

import DateTime
import Session


main :: IO ()
main = runFastCGIorCGI $ handleErrors $ withLogin process

process :: UserInfo -> DB.Connection -> CGI CGIResult
process userInfo conn = do
    fileName <- getInputFilename "file"
    contentType <- fromMaybe "application/octet-stream" `liftM` getInputContentType "file"
    file <- getInputFPS "file"
    comment <- getInput "comment"
    liftIO $ putContent conn (userId userInfo) fileName contentType file comment
    redirect "top.cgi"

putContent :: DB.Connection -> UserId -> Maybe String -> String -> Maybe B.ByteString -> Maybe String -> IO ()
putContent conn userId (Just fileName) contentType (Just content) comment = do
    path <- (randomIO :: IO Int) >>= return . sha1 . show
    B.writeFile ("contents/" ++ path) content
    updated <- getClockTime
    stmt <- DB.prepare conn sql
    DB.execute stmt [DB.toSql userId,
                     DB.toSql fileName,
                     DB.toSql contentType,
                     DB.toSql $ formatDateTime updated,
                     DB.toSql comment,
                     DB.toSql path]
    return ()
 where
     sql = "INSERT INTO data VALUES (NULL, ?, ?, ?, ?, ?, ?)"
putContent _ _ _ _ _ _ = throwIO $ IOException $ userError "Filename and content must be specified."
