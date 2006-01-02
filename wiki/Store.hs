module Store where

import System.Time


data PageMetadata = PageMetadata String ClockTime

class Store a where
    existPage  :: a -> String -> IO Bool
    getPage    :: a -> String -> IO String
    updatePage :: a -> String -> String -> IO ()
    removePage :: a -> String -> IO ()
    listPages  :: a -> IO [PageMetadata]
