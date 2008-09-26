value = length $ concat $ takeWhile (not . null) $ map v [1..]

v k = dropWhile (\n -> length (show n) < k) $ takeWhile (\n -> length (show n) <= k) $ map (^k) [1..]
