import Data.List

combination2 :: [Char] -> [[[Char]]]
combination2 candidates = concatMap (combination2' $ sort candidates) [1 .. length candidates - 1]

combination2' :: [Char] -> Int -> [[[Char]]]
combination2' candidates 0 = [[[], candidates]]
combination2' candidates n = nub $ concatMap f (combination2' candidates (n - 1))
  where
    f [f, s] = map (\c -> [sort $ f ++ [c], delete c s]) s

combination :: [Char] -> Int -> [[[Char]]]
combination candidates 2 = combination2 candidates
combination candidates n = concatMap f $ combination candidates (n - 1)
  where
    f :: [[Char]] -> [[[Char]]]
    f l = map ((init l) ++) $ combination2 $ last l
