module FunctorMonoid where

import Data.Kind
import Functor
import NaturalTransformation
import Prelude ()

type FunctorMonoidalCategory :: BinaturalTransformationType -> Constraint
class (BinaturalTransformation t) => FunctorMonoidalCategory t where
  type Unit t :: FunctorType

  assoc ::
    (Functor f, Functor g, Functor h) =>
    t f (t g h) ~> t (t f g) h
  assocInv ::
    (Functor f, Functor g, Functor h) =>
    t (t f g) h ~> t f (t g h)

  left :: (Functor f) => t (Unit t) f ~> f
  leftInv :: (Functor f) => f ~> t (Unit t) f

  right :: (Functor f) => t f (Unit t) ~> f
  rightInv :: (Functor f) => f ~> t f (Unit t)

type FunctorMonoidObject :: FunctorType -> Constraint
class
  ( Functor f,
    Functor (Tensor f f f),
    Functor (Unit (Tensor f)),
    FunctorMonoidalCategory (Tensor f)
  ) =>
  FunctorMonoidObject f
  where
  type Tensor f :: BinaturalTransformationType
  mu :: Tensor f f f ~> f
  eta :: Unit (Tensor f) ~> f
