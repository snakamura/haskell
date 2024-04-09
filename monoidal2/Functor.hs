module Functor where

import Data.Kind
import Prelude ()

type FunctorType :: Type
type FunctorType = Type -> Type

type Functor :: FunctorType -> Constraint
class Functor f where
  fmap :: (a -> b) -> (f a -> f b)

type BifunctorType :: Type
type BifunctorType = Type -> Type -> Type

type Bifunctor :: BifunctorType -> Constraint
class
  (forall a. Functor (f a)) =>
  Bifunctor f
  where
  bimap :: (a -> c) -> (b -> d) -> (f a b -> f c d)
