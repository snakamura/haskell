value = fst $ head $ dropWhile ((< 1000) . length . show .snd) $ zip [1..] fib

fib = 1:1:zipWith (+) fib (tail fib)
