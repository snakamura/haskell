value = last $ f 600851475143 [3..600851475142]

f _ []                     = []
f 1 _                      = []
f x (c:r) | x `mod` c == 0 = c:f (x `div` c) r
          | otherwise      = f x r
