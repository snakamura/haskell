module FunctorMonoid where

import Data.Kind
import Functor
import Functor2

type FunctorMonoidalCategory ::
  Bifunctor2Type ->
  Constraint
class (Bifunctor2 t, Functor (FunctorUnit t)) => FunctorMonoidalCategory t where
  type FunctorUnit t :: FunctorType

  assoc ::
    (Functor f, Functor g, Functor h) =>
    t f (t g h) ~> t (t f g) h
  assocInv ::
    (Functor f, Functor g, Functor h) =>
    t (t f g) h ~> t f (t g h)

  left :: (Functor f) => t (FunctorUnit t) f ~> f
  leftInv :: (Functor f) => f ~> t (FunctorUnit t) f

  right :: (Functor f) => t f (FunctorUnit t) ~> f
  rightInv :: (Functor f) => f ~> t f (FunctorUnit t)

type FunctorMonoidObject :: Bifunctor2Type -> FunctorType -> Constraint
class (FunctorMonoidalCategory t, Functor f) => FunctorMonoidObject t f where
  mu :: t f f ~> f
  eta :: FunctorUnit t ~> f

type FunctorMonoidHomomorphism :: FunctorType -> FunctorType -> Type
newtype FunctorMonoidHomomorphism m1 m2 = FunctorHom (m1 ~> m2)

class
  ( FunctorMonoidObject t m1,
    FunctorMonoidObject t m2
  ) =>
  FunctorMonoidHomomorphismLaws t m1 m2
  where
  preserveIdentity :: (Eq a) => FunctorMonoidHomomorphism m1 m2 -> a -> Bool
  preserveAppend :: (Eq a) => FunctorMonoidHomomorphism m1 m2 -> t m1 m1 a -> Bool
