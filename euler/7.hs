import Control.Monad.Reader
import Control.Monad.State

value = last $ take 10001 primes

primes = primes' [] $ 2:[3,5..]

primes' _      []                              = []
primes' found (c:candidates) | isPrime found c = c:primes' (found ++ [c]) candidates
                             | otherwise       = primes' found candidates

isPrime _      1 = False
isPrime primes n = let m = ceiling $ sqrt $ fromIntegral n
                   in and $ map (\p -> n `mod` p /= 0) $ takeWhile (<= m) primes


primesS = evalState (primesS' $ 2:[3,5..]) []

primesS' [] = return []
primesS' (c:candidates) = do
    b <- isPrimeS c
    if b
        then do
            modify (++ [c])
            liftM (c:) $ primesS' candidates
        else primesS' candidates

isPrimeS n = do
    primes <- get
    return $ isPrime primes n


primesR = runReader (primesR' $ 2:[3,5..]) []

primesR' [] = return []
primesR' (c:candidates) = do
    b <- isPrimeR c
    if b
        then local (++ [c]) $ liftM (c:) $ primesR' candidates
        else primesR' candidates

isPrimeR n = ask >>= return . flip isPrime n
