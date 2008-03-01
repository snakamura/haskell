import Control.Monad

value = head $ do
    a <- [1..333]
    b <- [1..500]
    let c = 1000 - a - b
    guard $ a < b
    guard $ b < c
    guard $ a^2 + b ^2 == c^2
    return $ a*b*c

{-
value = head $ [ a*b*(1000 - a - b) | a <- [1..333],
                                      b <- [1..500],
                                      a < b,
                                      b < (1000 - a - b),
                                      a^2 + b^2 == (1000 - a - b)^2 ]
-}
