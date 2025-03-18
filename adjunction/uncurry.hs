{-# LANGUAGE RankNTypes #-}

f :: a -> b -> c
f = undefined

f1 :: (a, b) -> c
f1 = uncurry f

f1' :: (,) a b -> c
f1' = f1

type F b = forall a. (,) a b

f1'' :: F b -> c
f1'' = f1'

f2 :: b -> a -> c
f2 = flip f

f2' :: b -> (a -> c)
f2' = f2

f2'' :: b -> (->) a c
f2'' = f2'

type G b = forall a. (->) a b

f2''' :: b -> G c
f2''' = f2''

-- f1'' =~ f2'''
-- F b -> c =~ b -> G c
