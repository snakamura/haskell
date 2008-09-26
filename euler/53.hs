value = length $ filter (> 1000000) [ combi n r | n <- [1..100], r <- [1..n] ]

combi n r = fac n `div` fac r `div` fac (n - r)

fac 0 = 1
fac n = n*fac (n - 1)
