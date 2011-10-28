{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main (main) where

import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Control.Exception (SomeException, bracket, catch)
import Data.Enumerator (($$), (=$), catchError, enumList, run_)
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET
import qualified Data.Text as T
import Network.HTTP.Types (headerContentType, status200, status500)
import Network.Wai (Application, Response(..), responseLBS)
import Network.Wai.Handler.Warp (run)
import Prelude hiding (catch)
import System.IO (IOMode(ReadMode), hClose, hSetEncoding, openFile, utf8)


main :: IO ()
main = run 8000 (handleError upper)
  where
    handleError app req = app req `catchError` (const $ return $ responseLBS status500 [headerContentType "text/html"] "<h1>Internal error</h1>")


upper :: Application
upper _ = do
    return $ ResponseEnumerator $ \f ->
        let handler (_ :: SomeException) = run_ $ enumList 1 [fromText "<h1>Internal error</h1>"] $$ f status500 [headerContentType "text/html"]
        in flip catch handler $ bracket (openFile "enumerator.hsx" ReadMode) hClose $ \h -> do
            hSetEncoding h utf8
            let i = f status200 [headerContentType "text/plain"]
            run_ $ ET.enumHandle h $$ EL.map T.toUpper =$ EL.map fromText =$ EL.isolate 10 =$ i
