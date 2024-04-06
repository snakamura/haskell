module FunctorDay where

import Control.Applicative qualified
import Data.Functor qualified
import Data.Maybe
import Functor
import FunctorMonoid
import NaturalTransformation
import Prelude (const, flip, ($), (.))

type Day :: FunctorType -> FunctorType -> FunctorType
data Day f g a = forall b c. Day (f b) (g c) (b -> c -> a)

instance Functor (Day f g) where
  fmap :: (a -> b) -> (Day f g a -> Day f g b)
  fmap ab (Day fb gc bca) = Day fb gc (\b c -> ab (bca b c))

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

newtype Identity a = Identity a

instance Functor Identity where
  fmap :: (a -> b) -> (Identity a -> Identity b)
  fmap ab (Identity a) = Identity (ab a)

-- (Hask -> Hask, Day, Identity) is a monoidal category

instance FunctorMonoidal Day where
  type Unit Day = Identity

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

instance Functor Maybe where
  fmap :: (a -> b) -> (Maybe a -> Maybe b)
  fmap ab (Just a) = Just (ab a)
  fmap _ Nothing = Nothing

instance FunctorMonoid Maybe where
  type Tensor Maybe = Day

  mu :: Day Maybe Maybe ~> Maybe
  mu (Day (Just b) (Just c) bca) = Just (bca b c)
  mu _ = Nothing

  eta :: Identity ~> Maybe
  eta (Identity a) = Just a

instance
  ( Data.Functor.Functor f,
    FunctorMonoid f,
    Tensor f ~ Day,
    Unit (Tensor f) ~ Identity
  ) =>
  Control.Applicative.Applicative f
  where
  (<*>) :: f (a -> b) -> (f a -> f b)
  (<*>) fab fa = mu $ Day fab fa (\ab a -> ab a)

  pure :: a -> f a
  pure = eta . Identity
