{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Blaze.ByteString.Builder.Char.Utf8 (fromChar, fromString, fromText)
import Control.Monad (join)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import Network.HTTP.Types (headerContentType, queryToQueryText, status200)
import Network.Wai (Application, Response(..), queryString)
import Network.Wai.Handler.Warp (run)


main :: IO ()
main = run 8000 hello


hello :: Application
hello req = do
  let query = queryToQueryText $ queryString req
      headers = [headerContentType "text/plain"]
      body = fromString "Hello, " `mappend`
             fromText (fromMaybe "(Unknown)" $ join $ lookup "name" query) `mappend`
             fromChar '!'
  return $ ResponseBuilder status200 headers body
