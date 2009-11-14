{-# LANGUAGE ImpredicativeTypes, Rank2Types #-}

f :: Maybe (forall a. [a] -> a) -> (Int, Char)
f (Just g) = (g [1, 2, 3], g ['a', 'b'])
f Nothing  = (99, 'z')
