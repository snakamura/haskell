module Fix where

import Data.Kind

type Fix :: (Type -> Type) -> Type
newtype Fix f = Fix (f (Fix f))

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . project

ana :: (Functor f) => (a -> f a) -> a -> Fix f
ana coalg = embed . fmap (ana coalg) . coalg

embed :: f (Fix f) -> Fix f
embed = Fix

project :: Fix f -> f (Fix f)
project (Fix ffixf) = ffixf

para :: (Functor f) => (f (Fix f, a) -> a) -> Fix f -> a
para @f @a phi = snd . cata alg
  where
    alg :: f (Fix f, a) -> (Fix f, a)
    alg x = (embed (fst <$> x), phi x)

apo :: (Functor f) => (a -> f (Either (Fix f) a)) -> a -> Fix f
apo @f @a psi = ana coalg . Right
  where
    coalg :: Either (Fix f) a -> f (Either (Fix f) a)
    coalg (Left nu) = Left <$> project nu
    coalg (Right a) = psi a
