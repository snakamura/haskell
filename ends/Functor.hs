module Functor where

import Data.Functor.Contravariant
import Data.Functor.Const
import Data.Kind
import Data.Profunctor

newtype WrappedFunctor f a b = WrappedFunctor (f b)

instance (Functor f) => Profunctor (WrappedFunctor f) where
  dimap :: (s -> a) -> (b -> t) -> (WrappedFunctor f a b -> WrappedFunctor f s t)
  dimap _ b2t (WrappedFunctor fb) = WrappedFunctor $ fmap b2t fb

newtype WrappedContravariant f a b = WrappedContravariant (f a)

instance (Contravariant f) => Profunctor (WrappedContravariant f) where
  dimap :: (s -> a) -> (b -> t) -> (WrappedContravariant f a b -> WrappedContravariant f s t)
  dimap s2a _ (WrappedContravariant fa) = WrappedContravariant (contramap s2a fa)

type Hom :: (Type -> Type) -> (Type -> Type) -> Type -> Type -> Type
newtype Hom f g a b = Hom (f a -> g b)

instance (Functor f, Functor g) => Profunctor (Hom f g) where
  dimap :: (s -> a) -> (b -> t) -> (Hom f g a b -> Hom f g s t)
  dimap s2a b2t (Hom h) = Hom (fmap b2t . h . fmap s2a)

instance (Functor g) => Functor (Hom (Const c) g a) where
  fmap :: (b -> t) -> (Hom (Const c) g a b -> Hom (Const c) g a t)
  fmap = rmap

newtype HomOp f g a b = HomOp (Hom f g b a)

instance (Functor f) => Contravariant (HomOp f (Const c) b) where
  contramap :: (s -> a) -> (HomOp f (Const c) b a -> HomOp f (Const c) b s)
  contramap s2a (HomOp hom)= HomOp (lmap s2a hom)
