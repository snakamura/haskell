module Mu where

import Data.Kind

type Mu :: (Type -> Type) -> Type
newtype Mu f where
  In :: (forall a. (f a -> a) -> a) -> Mu f

cata :: (Functor f) => (f a -> a) -> Mu f -> a
cata alg (In run) = run alg

ana :: (Functor f) => (a -> f a) -> a -> Mu f
ana coalg a = In (\alg -> let h x = alg (h <$> coalg x) in h a)

embed :: (Functor f) => f (Mu f) -> Mu f
embed fmuf = In $ \alg -> alg $ cata alg <$> fmuf

project :: (Functor f) => Mu f -> f (Mu f)
{-
project = para $ fmap fst
-}
project @f = snd . cata alg
  where
    alg :: f (Mu f, f (Mu f)) -> (Mu f, f (Mu f))
    alg x = (embed (fst <$> x), fst <$> x)

para :: (Functor f) => (f (Mu f, a) -> a) -> Mu f -> a
para @f @a phi = snd . cata alg
  where
    alg :: f (Mu f, a) -> (Mu f, a)
    alg x = (embed (fst <$> x), phi x)
