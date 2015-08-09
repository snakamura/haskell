import Data.List

combination2 :: [Char] -> [([Char], [Char])]
combination2 candidates = concatMap (combination2' $ sort candidates) [1 .. length candidates - 1]

combination2' :: [Char] -> Int -> [([Char], [Char])]
combination2' candidates 0 = [([], candidates)]
combination2' candidates n = nub $ concatMap f (combination2' candidates (n - 1))
  where
    f (f, s) = map (\c -> (sort $ f ++ [c], delete c s)) s

combination3 :: [Char] -> [([Char], [Char], [Char])]
combination3 candidates = concatMap f $ combination2 candidates
  where
    f :: ([Char], [Char]) -> [([Char], [Char], [Char])]
    f (f, s) = map (\(f', s') -> (f, f', s')) $ combination2 s
