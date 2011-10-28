{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.HTTP.Types (headerContentType, status200)
import Network.Wai (Application, Response(..))
import Network.Wai.Handler.Warp (run)


main :: IO ()
main = run 8000 hello


hello :: Application
hello _ = return $ ResponseFile status200 [headerContentType "text/plain"] "file.hs" Nothing
