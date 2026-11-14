module Linear where

import Data.Unrestricted.Linear
import Data.Vector qualified as V
import Data.Vector.Mutable.Linear qualified as MLV
import Prelude.Linear
import Prelude ()

main :: Int
main = complicated 10

complicated :: Int -> Int
complicated n = MLV.empty $ \v ->
  case MLV.freeze $ fill n v of
    Ur v' -> V.sum v'

fill :: Int -> MLV.Vector Int %1 -> MLV.Vector Int
fill n v = foldr step v [0 .. n - 1]
  where
    step :: Int %1 -> MLV.Vector Int %1 -> MLV.Vector Int
    step m v = case move m of
      Ur m' -> MLV.push (m' * 42) v

{-
complicated' :: Int -> Ur (MLV.Vector Int)
complicated' n = MLV.empty $ \v -> Ur (fill n v)
-}
{-
fill' :: Int -> MLV.Vector Int %1 -> MLV.Vector Int
fill' n v = foldr (\m v -> MLV.push (m * 42) v) v [0 .. n - 1]
-}
