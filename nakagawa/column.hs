column n = reverse $ takeWhile (/= ' ') $ map c [0..]
 where
     c 0 = cycle ['A'..'Z'] !! n
     c m = (replicate (sum $ map (26^) [1..m]) ' ' ++ cycle (concatMap (replicate (26^m)) ['A'..'Z'])) !! n
