import Data.List

combination :: [Char] -> [([Char], [Char])]
combination candidates = concatMap (combination' $ sort candidates) [0 .. length candidates]

combination' :: [Char] -> Int -> [([Char], [Char])]
combination' candidates 0 = [([], candidates)]
combination' candidates n = nub $ concatMap f (combination' candidates (n - 1))
  where
    f (f, s) = map (\c -> (sort $ f ++ [c], delete c s)) s
