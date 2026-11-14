module ST where

import Control.Monad.ST
import Data.Foldable
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV

main :: Int
main = runST $ complicated 10

complicated :: Int -> ST s Int
complicated n = do
  v <- MV.new n
  fill n v
  V.sum <$> V.freeze v

fill :: Int -> MV.MVector s Int -> ST s ()
fill n v = do
  for_ [0 .. n - 1] $ \m ->
    MV.write v m (m * 42)

{-
complicated' :: Int -> ST s (MV.MVector s Int)
complicated' n = do
  v <- MV.new n
  fill n v
  pure v
-}
