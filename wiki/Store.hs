module Store (PageMetadata(..),
              Store(..))
    where

import qualified System.Time as Time


data PageMetadata = PageMetadata {
                        name         :: String,
                        lastModified :: Time.ClockTime
                    }

class Store a where
    existPage  :: a -> String -> IO Bool
    getPage    :: a -> String -> IO String
    updatePage :: a -> String -> String -> IO ()
    removePage :: a -> String -> IO ()
    listPages  :: a -> IO [PageMetadata]
