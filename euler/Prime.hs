module Prime where

primes = primes' [] $ 2:[3,5..]

primes' _      []                              = []
primes' found (c:candidates) | isPrime found c = c:primes' (found ++ [c]) candidates
                             | otherwise       = primes' found candidates

isPrime _      1 = False
isPrime _      2 = True
isPrime primes n = let m = ceiling $ sqrt $ fromIntegral n
                   in and $ map (\p -> n `mod` p /= 0) $ takeWhile (<= m) primes
