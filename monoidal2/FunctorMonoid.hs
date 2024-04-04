module FunctorMonoid where

import Data.Kind
import Functor
import NaturalTransformation
import Prelude ()

type FunctorMonoid :: FunctorType -> Constraint
class (Functor f, Functor (Tensor f), Functor (Unit f)) => FunctorMonoid f where
  type Tensor f :: FunctorType
  type Unit f :: FunctorType
  mu :: Tensor f ~> f
  eta :: Unit f ~> f
