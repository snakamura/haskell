module FunctorMonoid where

import Data.Kind
import Functor
import NaturalTransformation
import Prelude ()

type FunctorMonoid :: FunctorType -> Constraint
class
  ( Functor f,
    Functor (Tensor f f f),
    Functor (Unit f),
    BinaturalTransformation (Tensor f)
  ) =>
  FunctorMonoid f
  where
  type Tensor f :: FunctorType -> FunctorType -> FunctorType
  type Unit f :: FunctorType
  mu :: Tensor f f f ~> f
  eta :: Unit f ~> f
