import Prime

main = print value

value = head $ map sum $ p 5

check a b = a < b &&
            isPrime primes (read $ show a ++ show b) &&
            isPrime primes (read $ show b ++ show a)

candidates = take 2000 primes

p 1 = map (:[]) candidates
p n = [ y:x | x <- p (n - 1), y <- candidates, and $ zipWith check x (repeat y) ]
