import Control.Monad.Fix (fix)
import Prelude hiding (sum)

sumF :: (Num a, Eq a) => (a -> a) -> a -> a
sumF _ 0 = 0
sumF f n = n + f (n - 1)

sum :: (Num a, Eq a) => a -> a
sum = fix sumF


sum5 :: (Num a, Eq a) => a
sum5 = sum 5
