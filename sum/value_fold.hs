import Data.List (foldl')
import Prelude hiding (sum)

sum :: (Num a, Enum a) => a -> a
sum n = foldl' (+) 0 [1 .. n]


sum5 :: (Num a, Enum a) => a
sum5 = sum 5
