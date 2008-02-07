{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed

f :: forall s. ST s (UArray (Int, Int) Bool)
f = do
    arr <- newArray ((0, 0), (100, 100)) True  :: ST s (STUArray s (Int, Int) Bool)
    writeArray arr (1, 1) False
    readArray arr (1, 1)
    freeze arr -- :: ST s (UArray (Int, Int) Bool)

g = runSTArray $ do
    arr <- newArray (0, 100) True
    writeArray arr 1 False
    readArray arr 2
    return arr

test = runST f
