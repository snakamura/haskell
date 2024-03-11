module FunctorMonoid where

import Data.Kind
import Functor
import NaturalTransformation

type FunctorMonoid :: FunctorType -> Constraint
class FunctorMonoid f where
  type Tensor f :: FunctorType
  type Id f :: FunctorType
  mu :: Tensor f ~> f
  eta :: Id f ~> f
