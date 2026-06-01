module Nu where

import Data.Kind

type Nu :: (Type -> Type) -> Type
data Nu f where
  Out :: (s -> f s) -> s -> Nu f

cata :: (Functor f) => (f a -> a) -> Nu f -> a
cata alg nu = alg $ cata alg <$> project nu

ana :: (Functor f) => (a -> f a) -> a -> Nu f
ana = Out

embed :: (Functor f) => f (Nu f) -> Nu f
embed = apo (fmap Left)
{-
embed = ana coalg . Left
  where
    coalg (Left f) = Right <$> f
    coalg (Right n) = Right <$> project n
-}

project :: (Functor f) => Nu f -> f (Nu f)
project (Out next state) = Out next <$> next state

apo :: (Functor f) => (a -> f (Either (Nu f) a)) -> a -> Nu f
apo psi = ana c . Right
  where
    c (Left n) = Left <$> project n
    c (Right a) = psi a
