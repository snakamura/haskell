{-# LANGUAGE ForeignFunctionInterface #-}
{-# INCLUDE "alarm.h" #-}

module Main (main) where

import Foreign.C.Types
import Foreign.Ptr

main :: IO ()
main = do c <- wrapCallback callback
          test c
          freeHaskellFunPtr c

callback :: CInt -> IO ()
callback n = do putStrLn $ "Haskell: " ++ show n
                mapM_ return [1..10000000]

foreign import ccall "test" test :: FunPtr (CInt -> IO ()) -> IO ()
foreign import ccall "wrapper" wrapCallback :: (CInt -> IO ()) -> IO (FunPtr (CInt -> IO ()))
