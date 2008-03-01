value = ((powerWithModulus (10^10) 2 7830457) * 28433 + 1) `mod` 10^10

powerWithModulus mo n k | odd k      = powerMod mo n n (k-1)
                        | otherwise  = powerMod mo 1 n k

powerMod mo aux val k
   | k == 1    = (aux * val) `mod` mo
   | odd k     = ((powerMod mo $! ((aux*val) `mod` mo)) $! (val^2 `mod` mo)) $! (k `quot` 2)
   | even k    = (powerMod mo aux $! (val^2 `mod` mo)) $! (k `quot` 2)
