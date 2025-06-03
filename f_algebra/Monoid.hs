module Monoid where

import Prelude hiding (Monoid)

data Monoid a = Append a a
              | Empty

instance Functor Monoid where
    fmap f (Append n m) = Append (f n) (f m)
    fmap _ Empty = Empty

sumIntAlg :: Monoid Int -> Int
sumIntAlg (Append m n) = m + n
sumIntAlg Empty = 0

productIntAlg :: Monoid Int -> Int
productIntAlg (Append m n) = m * n
productIntAlg Empty = 1

v1, v2 :: Int
v1 = sumIntAlg (Append 1 2)
v2 = productIntAlg (Append 1 2)

sumMonoidIntAlg :: Monoid (Monoid Int) -> Monoid Int
sumMonoidIntAlg (Append m n) = Append (sumIntAlg m) (sumIntAlg n)
sumMonoidIntAlg Empty = Empty

v3 :: Int
v3 = sumIntAlg $ sumMonoidIntAlg $ Append (Append 1 2) Empty

sumMonoidMonoidIntAlg :: Monoid (Monoid (Monoid Int)) -> Monoid (Monoid Int)
sumMonoidMonoidIntAlg (Append m n) = Append (sumMonoidIntAlg m) (sumMonoidIntAlg n)
sumMonoidMonoidIntAlg Empty = Empty

v4 :: Int
v4 = sumIntAlg $ sumMonoidIntAlg $ sumMonoidMonoidIntAlg $ Append (Append (Append 1 2) (Append 3 4)) Empty

newtype Fix f = In (f (Fix f))

out :: Fix f -> f (Fix f)
out (In f) = f

initialAlg :: Monoid (Fix Monoid) -> Fix Monoid
initialAlg = In

m0, m1 :: Fix Monoid
m0 = In Empty
m1 = In (Append (In Empty) (In (Append (In Empty) (In Empty))))

homInitialToSumInt :: Fix Monoid -> Int
homInitialToSumInt = sumIntAlg . fmap homInitialToSumInt . out

i0, i1 :: Int
i0 = homInitialToSumInt m0
i1 = homInitialToSumInt m1

homInitialToInt :: (Monoid Int -> Int) -> Fix Monoid -> Int
homInitialToInt alg = alg . fmap (homInitialToInt alg) . out

hom :: (Monoid a -> a) -> Fix Monoid -> a
hom alg = alg . fmap (hom alg) . out

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . out
