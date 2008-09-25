import Data.List
import Data.Ord

import Prime

value = sum $ take 11 $ drop 4 $ filter truncatable primes
    where
      truncatable n = let s = show n
                      in all (is . read) $ filter (/= []) $ sortBy (comparing length) $ tails s ++ inits s
      is n = maybe False (== n) $ find (>= n) primes
