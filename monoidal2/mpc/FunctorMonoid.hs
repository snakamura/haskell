module FunctorMonoid where

import Data.Kind
import Functor
import Functor2
import Prelude ()

type FunctorMonoidalCategory ::
  Bifunctor2Type ->
  FunctorType ->
  Constraint
class
  (Bifunctor2 t) =>
  FunctorMonoidalCategory t u
    | t -> u
  where
  assoc ::
    (Functor f, Functor g, Functor h) =>
    t f (t g h) ~> t (t f g) h
  assocInv ::
    (Functor f, Functor g, Functor h) =>
    t (t f g) h ~> t f (t g h)

  left :: (Functor f) => t u f ~> f
  leftInv :: (Functor f) => f ~> t u f

  right :: (Functor f) => t f u ~> f
  rightInv :: (Functor f) => f ~> t f u

type FunctorMonoidObject ::
  Bifunctor2Type ->
  FunctorType ->
  FunctorType ->
  Constraint
class
  ( FunctorMonoidalCategory t u,
    Functor u,
    Functor f
  ) =>
  FunctorMonoidObject t u f
  where
  mu :: t f f ~> f
  eta :: u ~> f
