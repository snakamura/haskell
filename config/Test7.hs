module Main (main) where

import Control.Monad.State

main :: IO ()
main = do s <- execStateT (add 10) 5
          print s

add :: Int -> StateT Int IO ()
add n = do modify (+n)
           liftIO $ print n
