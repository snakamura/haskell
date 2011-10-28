{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Blaze.ByteString.Builder.Char.Utf8 (fromChar, fromString, fromText)
import Control.Monad (join)
import Data.ByteString.Class (toStrictByteString)
import Data.Enumerator.Binary (consume)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import Network.HTTP.Types (headerContentType, methodPost, parseQueryText, status200)
import Network.Wai (Application, Response(..), requestMethod)
import Network.Wai.Handler.Warp (run)


main :: IO ()
main = run 8000 hello


hello :: Application
hello req 
    | requestMethod req == methodPost = do
        requestBody <- consume
        let query = parseQueryText $ toStrictByteString requestBody
            body = fromString "Hello, " `mappend`
                   fromText (fromMaybe "(Unknown)" $ join $ lookup "name" query) `mappend`
                   fromChar '!'
        return $ ResponseBuilder status200 headers body
    | otherwise = do
        let body = fromString "<form method=\"post\" action=\".\"><input type=\"text\" name=\"name\"/></form>"
        return $ ResponseBuilder status200 headers body
  where
    headers = [headerContentType "text/html"]
