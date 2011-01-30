value = sum $ map v [3..1000]

v a = let n = ((a - 1) `div` 2)
      in 2*n*a

