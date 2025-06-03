module FreeMonoid where

data FreeMonoid a r = Append r r
                    | Empty
                    | Lift a

instance Functor (FreeMonoid a) where
    fmap f (Append n m) = Append (f n) (f m)
    fmap _ Empty = Empty
    fmap _ (Lift n) = Lift n

sumIntAlg :: FreeMonoid Int Int -> Int
sumIntAlg (Append m n) = m + n
sumIntAlg Empty = 0
sumIntAlg (Lift n) = n

productIntAlg :: FreeMonoid Int Int -> Int
productIntAlg (Append m n) = m * n
productIntAlg Empty = 1
productIntAlg (Lift n) = n

v1, v2 :: Int
v1 = sumIntAlg (Lift 1)
v2 = productIntAlg (Lift 1)

sumMonoidIntAlg :: FreeMonoid Int (FreeMonoid Int Int) -> FreeMonoid Int Int
sumMonoidIntAlg (Append m n) = Append (sumIntAlg m) (sumIntAlg n)
sumMonoidIntAlg Empty = Empty
sumMonoidIntAlg (Lift n) = Lift n

v3 :: Int
v3 = sumIntAlg $ sumMonoidIntAlg $ Append (Lift 1) (Lift 2)

sumMonoidMonoidIntAlg :: FreeMonoid Int (FreeMonoid Int (FreeMonoid Int Int)) -> FreeMonoid Int (FreeMonoid Int Int)
sumMonoidMonoidIntAlg (Append m n) = Append (sumMonoidIntAlg m) (sumMonoidIntAlg n)
sumMonoidMonoidIntAlg Empty = Empty
sumMonoidMonoidIntAlg (Lift n) = Lift n

v4 :: Int
v4 = sumIntAlg $ sumMonoidIntAlg $ sumMonoidMonoidIntAlg $ Append (Append (Lift 1) (Lift 2)) (Append (Lift 3) (Lift 4))

newtype Fix f = In (f (Fix f))

out :: Fix f -> f (Fix f)
out (In f) = f

initialAlg :: FreeMonoid a (Fix (FreeMonoid a)) -> Fix (FreeMonoid a)
initialAlg = In

m0, m1, m2, m3 :: Fix (FreeMonoid Int)
m0 = In Empty
m1 = In (Lift 1)
m2 = In (Append (In (Append (In (Lift 1)) (In (Lift 2)))) (In Empty))
m3 = In (Append (In (Append (In (Lift 1)) (In (Append (In (Lift 2)) (In (Lift 3)))))) (In Empty))

homInitialToSumInt :: Fix (FreeMonoid Int) -> Int
homInitialToSumInt = sumIntAlg . fmap homInitialToSumInt . out

i0, i1, i2, i3 :: Int
i0 = homInitialToSumInt m0
i1 = homInitialToSumInt m1
i2 = homInitialToSumInt m2
i3 = homInitialToSumInt m3

homInitialToInt :: (FreeMonoid Int Int -> Int) -> Fix (FreeMonoid Int) -> Int
homInitialToInt alg = alg . fmap (homInitialToInt alg) . out

hom :: (FreeMonoid a a -> a) -> Fix (FreeMonoid a) -> a
hom alg = alg . fmap (hom alg) . out

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . out

i0', i1', i2' :: Int
i0' = cata sumIntAlg m0
i1' = cata sumIntAlg m1
i2' = cata sumIntAlg m2
