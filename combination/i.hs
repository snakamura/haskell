import Data.List

combination :: [Char] -> Int -> [[[Char]]]
combination items 2 = concatMap (map toList . split (sort items)) [1 .. length items - 1]
  where
    toList (a, b) = [a, b]
combination items n = concatMap f $ combination items (n - 1)
  where
    f :: [[Char]] -> [[[Char]]]
    f l = map ((init l) ++) $ combination (last l) 2

split :: [Char] -> Int -> [([Char], [Char])]
split items 0 = [([], items)]
split items n = nub $ concatMap f (split items (n - 1))
  where
    f (f, s) = map (\c -> (sort $ f ++ [c], delete c s)) s
