import Data.List

import Prime

value = length $ filter f $ takeWhile (< 1000000) primes
    where
      f = all (isPrime primes) . tail . rotations

rotations n = map read $ rotations' $ show n

rotations' s = init $ zipWith (++) (tails s) (inits s)
