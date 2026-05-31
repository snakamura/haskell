module Mu where

import Data.Kind

type Mu :: (Type -> Type) -> Type
newtype Mu f where
  Mu :: (forall a. (f a -> a) -> a) -> Mu f

cata :: (Functor f) => (f a -> a) -> Mu f -> a
cata alg (Mu f) = f alg

ana :: (Functor f) => (a -> f a) -> a -> Mu f
ana coalg a = Mu (\alg -> let h x = alg (h <$> coalg x) in h a)

embed :: (Functor f) => f (Mu f) -> Mu f
embed fmuf = Mu $ \alg -> alg $ cata alg <$> fmuf

project :: (Functor f) => Mu f -> f (Mu f)
project = snd . cata phi
  where
    phi x = (embed (fst <$> x), fst <$> x)

para :: (Functor f) => (f (Mu f, a) -> a) -> Mu f -> a
para alg = snd . cata phi
  where
    phi x = (embed (fst <$> x), alg x)
