import Data.List
import Data.Ord

import Prime

value = let (_, (a, b)) = maximumBy (comparing fst) $
                              [ (v, (a, b)) | a <- [-999..999],
                                              b <- [-999..999],
                                              isPrime primes (abs b),
                                              let v = values a b,
                                              v >= 40]
        in a*b

values a b = length $ takeWhile (\n -> n > 0 && isPrime primes n) $ map (v a b) [0..]

v a b n = n^2 + a*n + b
