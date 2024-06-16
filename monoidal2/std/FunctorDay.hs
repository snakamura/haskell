module FunctorDay where

import Data.Functor.Classes
import Data.Functor.Day
import Data.Functor.Identity
import Data.Maybe
import Functor2
import FunctorMonoid

instance (Functor f) => Functor2 (Day f) where
  ntmap ::
    (Functor g, Functor h) =>
    (g ~> h) ->
    (Day f g ~> Day f h)
  ntmap gh (Day f g bca) = Day f (gh g) bca

instance
  (forall f. (Functor f) => Functor2 (Day f)) =>
  Bifunctor2 Day
  where
  bintmap ::
    (Functor f, Functor g, Functor h, Functor i) =>
    (f ~> h) ->
    (g ~> i) ->
    (Day f g ~> Day h i)
  bintmap fh gi (Day f g bca) = Day (fh f) (gi g) bca

-- (Hask -> Hask, Day, Identity) is a monoidal category

instance FunctorMonoidalCategory Day where
  type FunctorUnit Day = Identity

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

instance FunctorMonoidObject Day Maybe where
  mu :: Day Maybe Maybe ~> Maybe
  mu (Day (Just b) (Just c) bca) = Just (bca b c)
  mu _ = Nothing

  eta :: Identity ~> Maybe
  eta (Identity a) = Just a

instance FunctorMonoidObject Day [] where
  mu :: Day [] [] ~> []
  mu (Day bs cs bca) = [bca b c | b <- bs, c <- cs]

  eta :: Identity ~> []
  eta (Identity a) = [a]

instance
  {-# OVERLAPPABLE #-}
  ( Functor f,
    FunctorMonoidObject Day f
  ) =>
  Applicative f
  where
  (<*>) :: f (a -> b) -> (f a -> f b)
  (<*>) fab fa = mu $ Day fab fa (\ab a -> ab a)

  pure :: a -> f a
  pure = eta @Day . Identity

instance
  ( FunctorMonoidObject Day m1,
    FunctorMonoidObject Day m2,
    Eq1 m2
  ) =>
  FunctorMonoidHomomorphismLaws Day m1 m2
  where
  preserveIdentity :: (Eq a) => FunctorMonoidHomomorphism m1 m2 -> a -> Bool
  preserveIdentity (FunctorHom t) a = t (eta @Day (Identity a)) `eq1` eta @Day (Identity a)

  preserveAppend :: (Eq a) => FunctorMonoidHomomorphism m1 m2 -> Day m1 m1 a -> Bool
  preserveAppend (FunctorHom t) day@(Day fb fc bca) = t (mu day) `eq1` mu (Day (t fb) (t fc) bca)

homMaybeToList :: FunctorMonoidHomomorphism Maybe []
homMaybeToList = FunctorHom maybeToList

testPreserveIdentity, testPreserveAppend :: Bool
testPreserveIdentity = preserveIdentity @Day homMaybeToList 'a'
testPreserveAppend = preserveAppend homMaybeToList (Day (Just @Int 1) (Just 2) (+))
