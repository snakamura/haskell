import Data.List
import Data.Numbers.Primes
import Data.Ord

value = fst $ head $ reverse $ sortBy (comparing snd) $ [ (s, l) | p <- takeWhile (< (1000000 `div` 500)) primes,
                                                                   l <- [500..600],
                                                                   let c = take l $ dropWhile (< p) primes
                                                                       s = sum c,
                                                                   s < 1000000,
                                                                   isPrime s]
