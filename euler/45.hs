import Data.List
import Data.Maybe

{-
value = head $ filter f $ drop 285 triangles
    where
      f v = contain v (drop 165 pentagonals) && contain v (drop 143 hexagonals)
-}

value = next triangles pentagonals hexagonals !! 2

next tss@(t:ts) pss@(p:ps) hss@(h:hs) | t == p && p == h = p:next ts ps hs
                                      | t <= p && t <= h = next ts pss hss
                                      | p <= t && p <= h = next tss ps hss
                                      | otherwise        = next tss pss hs

triangle n = n*(n + 1) `div` 2
triangles = map triangle [1..]

pentagonal n = n*(3*n - 1) `div` 2
pentagonals = map pentagonal [1..]

hexagonal n = n*(2*n - 1)
hexagonals = map hexagonal [1..]

contain v l = fromJust (find (\n -> n >= v) l) == v
