module Nu where

import Data.Kind

type Nu :: (Type -> Type) -> Type
data Nu f where
  Out :: (a -> f a) -> a -> Nu f

cata :: (Functor f) => (f a -> a) -> Nu f -> a
cata alg = alg . fmap (cata alg) . project

ana :: (Functor f) => (a -> f a) -> a -> Nu f
ana = Out

embed :: (Functor f) => f (Nu f) -> Nu f
{-
embed = apo (fmap Left)
-}
embed @f = ana coalg . Right
  where
    coalg :: Either (Nu f) (f (Nu f)) -> f (Either (Nu f) (f (Nu f)))
    coalg (Left nu) = Left <$> project nu
    coalg (Right fnuf) = Left <$> fnuf

project :: (Functor f) => Nu f -> f (Nu f)
project (Out coalg value) = Out coalg <$> coalg value

apo :: (Functor f) => (a -> f (Either (Nu f) a)) -> a -> Nu f
apo @f @a psi = ana coalg . Right
  where
    coalg :: Either (Nu f) a -> f (Either (Nu f) a)
    coalg (Left nu) = Left <$> project nu
    coalg (Right a) = psi a
