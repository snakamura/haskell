value = sum $ filter even $ takeWhile (<= 1000000) fib

fib = 1:2:zipWith (+) fib (tail fib)
