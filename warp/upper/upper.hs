{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Blaze.ByteString.Builder.Char.Utf8 (fromLazyText)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Enumerator (($$), (=$), run_)
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET
import Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types (headerContentType, status200)
import Network.Wai (Application, Response(..))
import Network.Wai.Handler.Warp (run)
import System.IO (IOMode(ReadMode), hClose, hSetEncoding, openFile, utf8)


main :: IO ()
main = run 8000 upper


upper :: Application
upper _ = do
    t <- liftIO $ bracket (openFile "upper.hs" ReadMode) hClose $ \h -> do
           hSetEncoding h utf8
           run_ $ ET.enumHandle h $$ EL.map T.toUpper =$ EL.take 10
    return $ ResponseBuilder status200 [headerContentType "text/plain"] $ fromLazyText $ TL.fromChunks $ intersperse "\n" t
