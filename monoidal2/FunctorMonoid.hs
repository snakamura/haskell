module FunctorMonoid where

import Data.Kind
import Functor
import NaturalTransformation
import Prelude ()

type FunctorMonoid :: FunctorType -> Constraint
class Functor f => FunctorMonoid f where
  type Tensor f :: FunctorType
  type Id f :: FunctorType
  mu :: Tensor f ~> f
  eta :: Id f ~> f
