module Functor where

import Data.Profunctor
import Data.Functor.Contravariant

newtype WrappedFunctor f a b = WrappedFunctor (f b)

instance (Functor f) => Profunctor (WrappedFunctor f) where
  dimap :: (s -> a) -> (b -> t) -> (WrappedFunctor f a b -> WrappedFunctor f s t)
  dimap _ b2t (WrappedFunctor fb) = WrappedFunctor $ fmap b2t fb

newtype WrappedContravariant f a b = WrappedContravariant (f a)

instance (Contravariant f) => Profunctor (WrappedContravariant f) where
  dimap :: (s -> a) -> (b -> t) -> (WrappedContravariant f a b -> WrappedContravariant f s t)
  dimap s2a _ (WrappedContravariant fa) = WrappedContravariant (contramap s2a fa)
