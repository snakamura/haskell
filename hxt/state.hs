{-# LANGUAGE Arrows #-}

module Main (main) where

import Control.Monad
import System.Environment
import Text.XML.HXT.Arrow

main :: IO ()
main = do
    args <- getArgs
    let src = args !! 0
        tag = args !! 1
    (s, _) <- runIOSLA (readDocument [(a_validate, v_0)] src >>> count tag) (initialState 0) undefined
    print $ xio_userState s
    return ()

count tag = (hasName tag >>> changeUserState (const (+1)) >>> none) <+>
            (processChildren $ count tag)
