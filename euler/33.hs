import Data.List
import Data.Ratio

value = denominator $ foldl1 (*) $ [ n % d | d <- [10..99],
                                             n <- [10..d - 1],
                                             or (map (comp (n, d)) (cancel n d)) ]

comp (x, y) (a, b) = x % y == a % b

cancel n d = let ns = show n
                 ds = show d
                 cancel' c = (read $ delete c ns, read $ delete c ds)
             in filter (\(x, y) -> x /= 0 && y /= 0) $ map cancel' $ commons ns ds

commons []     _              = []
commons (c:cs) r | c /= '0' && c `elem` r = c:commons cs r
                 | otherwise              = commons cs r
