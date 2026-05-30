module Mu where

import Data.Kind

type Mu :: (Type -> Type) -> Type
newtype Mu f where
  Mu :: (forall a. (f a -> a) -> a) -> Mu f

cata :: (Functor f) => (f a -> a) -> Mu f -> a
cata alg (Mu f) = f alg

ana :: (Functor f) => (a -> f a) -> a -> Mu f
ana coalg a = Mu (\alg -> let h x = alg (h <$> coalg x) in h a)
