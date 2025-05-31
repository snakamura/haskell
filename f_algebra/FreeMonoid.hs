module FreeMonoid where

data FreeMonoid a r = Append r r
                    | Empty
                    | Lift a

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

m0, m1, m2 :: FreeMonoid Int (Fix (FreeMonoid Int))
m0 = Empty
m1 = (Append (In Empty) (In Empty))
m2 = (Append (In (Lift 1)) (In (Lift 2)))

instance Functor (FreeMonoid a) where
    fmap f (Append n m) = Append (f n) (f m)
    fmap _ Empty = Empty
    fmap _ (Lift n) = Lift n

homInitialToInt :: (FreeMonoid Int Int -> Int) -> Fix (FreeMonoid Int) -> Int
homInitialToInt alg = alg . fmap (homInitialToInt alg) . out

v0', v1', v2' :: Int
v0' = homInitialToInt sumIntAlg (In m0)
v1' = homInitialToInt sumIntAlg (In m1)
v2' = homInitialToInt sumIntAlg (In m2)

hom :: Functor f => (f a -> a) -> Fix f -> a
hom alg = alg . fmap (hom alg) . out
