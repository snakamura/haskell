import Prime

value = snd $ head $ dropWhile (\(r, _) -> r >= 0.1) $ tail $ zip ratios [1, 3..]


ratios = map (\(n, d) -> fromIntegral n / fromIntegral d) r
    where
      r = (0, 1):zipWith (\p (n, d) -> (n + length (filter id p), d + length p)) values r

values = map (map (isPrime primes) . corners) [3, 5..]

corners 1 = [1]
corners n = [n^2, n^2 - (n - 1), n^2 - (n - 1)*2, n^2 - (n - 1)*3]
