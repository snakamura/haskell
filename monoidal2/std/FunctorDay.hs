module FunctorDay where

import Data.Functor.Day
import Data.Functor.Identity
import FunctorMonoid
import NaturalTransformation

instance (Functor f) => NaturalTransformation (Day f) where
  ntmap ::
    (Functor g, Functor h) =>
    (g ~> h) ->
    (Day f g ~> Day f h)
  ntmap gh (Day f g bca) = Day f (gh g) bca

instance
  (forall f. (Functor f) => NaturalTransformation (Day f)) =>
  BinaturalTransformation Day
  where
  bintmap ::
    (Functor f, Functor g, Functor h, Functor i) =>
    (f ~> h) ->
    (g ~> i) ->
    (Day f g ~> Day h i)
  bintmap fh gi (Day f g bca) = Day (fh f) (gi g) bca

-- (Hask -> Hask, Day, Identity) is a monoidal category

instance FunctorMonoidalCategory Day Identity where
  assoc ::
    (Functor f, Functor g, Functor h) =>
    Day f (Day g h) ~> Day (Day f g) h
  assoc (Day fb (Day gc hd cde) bea) =
    Day (Day fb gc (,)) hd (\(b, c) d -> bea b (cde c d))

  assocInv ::
    (Functor f, Functor g, Functor h) =>
    Day (Day f g) h ~> Day f (Day g h)
  assocInv (Day (Day fb gc bcd) he dea) =
    Day fb (Day gc he (,)) (\b (c, e) -> dea (bcd b c) e)

  left :: (Functor f) => Day Identity f ~> f
  left (Day (Identity b) fc bca) = fmap (bca b) fc

  leftInv :: (Functor f) => f ~> Day Identity f
  leftInv fa = Day (Identity ()) fa (flip const)

  right :: (Functor f) => Day f Identity ~> f
  right (Day fb (Identity c) bca) = fmap (flip bca c) fb

  rightInv :: (Functor f) => f ~> Day f Identity
  rightInv fa = Day fa (Identity ()) const

instance FunctorMonoidObject Day Identity Maybe where
  mu :: Day Maybe Maybe ~> Maybe
  mu (Day (Just b) (Just c) bca) = Just (bca b c)
  mu _ = Nothing

  eta :: Identity ~> Maybe
  eta (Identity a) = Just a

instance
  {-# OVERLAPPABLE #-}
  ( Functor f,
    FunctorMonoidObject Day Identity f
  ) =>
  Applicative f
  where
  (<*>) :: f (a -> b) -> (f a -> f b)
  (<*>) fab fa = mu $ Day fab fa (\ab a -> ab a)

  pure :: a -> f a
  pure = eta @Day . Identity
