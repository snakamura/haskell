import Data.List

import Prime


value = head $ filter (isPrime primes) $ filter isPandigital [7654321, 7654320..]

isPandigital n = and $ zipWith (==) (sort (show n)) ['1'..]
