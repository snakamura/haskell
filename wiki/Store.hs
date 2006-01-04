module Store (PageMetadata(..),
              Store(..))
    where

import System.Time


data PageMetadata = PageMetadata {
                        name         :: String,
                        lastModified :: ClockTime
                    }

class Store a where
    existPage  :: a -> String -> IO Bool
    getPage    :: a -> String -> IO String
    updatePage :: a -> String -> String -> IO ()
    removePage :: a -> String -> IO ()
    listPages  :: a -> IO [PageMetadata]
