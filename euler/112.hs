import Data.List

value = snd $ head $ dropWhile (\(n, d) -> n*100 `div` d < 99) ratios

ratios = (0, 1):zipWith (\(n, d) b -> (n + if b then 1 else 0, d + 1)) ratios (tail values)

values = map bouncy [1..]

bouncy n = let s = show n
               i = sort s
               d = reverse i
           in s /= i && s /= d
