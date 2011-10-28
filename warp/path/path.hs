{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Blaze.ByteString.Builder.Char.Utf8 (fromChar, fromString, fromText)
import Control.Monad (join)
import Data.ByteString.Class (toStrictByteString)
import Data.Enumerator.Binary (consume)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import qualified Data.Text as T
import Network.HTTP.Types (headerContentType, parseQueryText, status200, status404)
import Network.Wai (Application, Response(..), pathInfo, responseLBS)
import Network.Wai.Handler.Warp (run)


main :: IO ()
main = run 8000 $ dispatch dispatchTable


type DispatchTable = [([T.Text], Application)]


dispatchTable :: DispatchTable
dispatchTable = [([],        root ),
                 (["hello"], hello)]


dispatch :: DispatchTable -> Application
dispatch table req =
    case lookup (pathInfo req) table of
      Just app -> app req
      Nothing -> return $ responseLBS status404 [headerContentType "text/html"] "<h1>Not found</h1>"


root :: Application
root _ = do
    let headers = [headerContentType "text/html"]
        body = fromString "<form method=\"post\" action=\"./hello\"><input type=\"text\" name=\"name\"/></form>"
    return $ ResponseBuilder status200 headers body
    


hello :: Application
hello _ = do
    requestBody <- consume
    let query = parseQueryText $ toStrictByteString requestBody
        headers = [headerContentType "text/html"]
        body = fromString "Hello, " `mappend`
               fromText (fromMaybe "(Unknown)" $ join $ lookup "name" query) `mappend`
               fromChar '!' `mappend`
               fromString "<br /><a href=\"..\">Back</a>"
    return $ ResponseBuilder status200 headers body
