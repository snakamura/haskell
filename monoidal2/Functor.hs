module Functor where

import Data.Kind
import Prelude ()

type FunctorType = Type -> Type

type Functor :: (Type -> Type) -> Constraint
class Functor f where
  fmap :: (a -> b) -> (f a -> f b)

type Bifunctor :: (Type -> Type -> Type) -> Constraint
class
  (forall a. Functor (f a)) =>
  Bifunctor f
  where
  bimap :: (a -> c) -> (b -> d) -> (f a b -> f c d)
