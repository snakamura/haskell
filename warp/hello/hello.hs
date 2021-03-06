{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.HTTP.Types (headerContentType, status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)


main :: IO ()
main = run 8000 hello


hello :: Application
hello _ = return $ responseLBS status200 [headerContentType "text/plain"] "Hello, world!"
