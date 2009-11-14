{-# LANGUAGE Rank2Types #-}

newtype MaybeWrapper = MaybeWrapper (forall a. Maybe ([a] -> a))

f :: MaybeWrapper -> (Int, Char)
f (MaybeWrapper g) = (h g [1, 2, 3] 99, h g ['a', 'b'] 'z')
  where
    h (Just f) l _ = f l
    h Nothing  _ d = d
