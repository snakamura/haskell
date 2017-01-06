import Prelude hiding (sum)

sum :: (Num a, Eq a) => a -> a
sum 0 = 0
sum n = n + sum (n - 1)


sum5 :: (Num a, Eq a) => a
sum5 = sum 5
