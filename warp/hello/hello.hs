{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Network.HTTP.Types (headerContentType, status200)
import Network.Wai (Application, Response(..))
import Network.Wai.Handler.Warp (run)


main :: IO ()
main = run 8000 hello


hello :: Application
hello _ = return $ ResponseBuilder status200 [headerContentType "text/plain"] $ fromString "Hello, world!"
