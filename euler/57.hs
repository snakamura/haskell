import Control.Arrow

value = length $ filter id $ take 1000 $ zipWith (\n d -> length (show n) > length (show d)) numerators denominators

numerators = 3:zipWith (+) denominators (tail denominators)
denominators = 2:zipWith (+) denominators numerators
