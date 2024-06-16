module FunctorProduct where

import Control.Applicative
import Data.Functor.Classes
import Data.Functor.Product
import Data.Maybe
import Data.Proxy
import Functor2
import FunctorMonoid

instance (Functor f) => Functor2 (Product f) where
  ntmap ::
    (Functor g, Functor h) =>
    (g ~> h) ->
    (Product f g ~> Product f h)
  ntmap gh (Pair fa ga) = Pair fa (gh ga)

instance
  (forall f. (Functor f) => Functor2 (Product f)) =>
  Bifunctor2 Product
  where
  bintmap ::
    (Functor f, Functor g, Functor h, Functor i) =>
    (f ~> h) ->
    (g ~> i) ->
    (Product f g ~> Product h i)
  bintmap fh gi (Pair fa ga) = Pair (fh fa) (gi ga)

-- (Hask x Hask, Product, Proxy) is a monoidal category

instance FunctorMonoidalCategory Product where
  type FunctorUnit Product = Proxy

  assoc ::
    (Functor f, Functor g, Functor h) =>
    Product f (Product g h)
      ~> Product
           (Product f g)
           h
  assoc (Pair fa (Pair ga ha)) = Pair (Pair fa ga) ha

  assocInv ::
    (Functor f, Functor g, Functor h) =>
    Product (Product f g) h ~> Product f (Product g h)
  assocInv (Pair (Pair fa ga) ha) = (Pair fa (Pair ga ha))

  left :: (Functor f) => Product Proxy f ~> f
  left (Pair Proxy fa) = fa

  leftInv :: (Functor f) => f ~> Product Proxy f
  leftInv fa = Pair Proxy fa

  right :: (Functor f) => Product f Proxy ~> f
  right (Pair fa Proxy) = fa

  rightInv :: (Functor f) => f ~> Product f Proxy
  rightInv fa = Pair fa Proxy

instance FunctorMonoidObject Product Maybe where
  mu :: Product Maybe Maybe ~> Maybe
  mu (Pair (Just a) _) = Just a
  mu (Pair _ (Just a)) = Just a
  mu _ = Nothing

  eta :: Proxy ~> Maybe
  eta _ = Nothing

instance FunctorMonoidObject Product [] where
  mu :: Product [] [] ~> []
  mu (Pair as bs) = as <> bs

  eta :: Proxy ~> []
  eta _ = []

instance
  {-# OVERLAPPABLE #-}
  ( Functor f,
    Applicative f,
    FunctorMonoidObject Product f
  ) =>
  Alternative f
  where
  (<|>) :: f a -> f a -> f a
  (<|>) fa1 fa2 = mu (Pair fa1 fa2)

  empty :: f a
  empty = eta @Product Proxy

instance
  ( FunctorMonoidObject Product m1,
    FunctorMonoidObject Product m2,
    Eq1 m2
  ) =>
  FunctorMonoidHomomorphismLaws Product m1 m2
  where
  preserveIdentity :: forall a. (Eq a) => FunctorMonoidHomomorphism m1 m2 -> a -> Bool
  preserveIdentity (FunctorHom t) _ = t (eta @Product (Proxy @a)) `eq1` eta @Product (Proxy @a)

  preserveAppend :: (Eq a) => FunctorMonoidHomomorphism m1 m2 -> Product m1 m1 a -> Bool
  preserveAppend (FunctorHom t) pair@(Pair fa fb) = t (mu pair) `eq1` mu (Pair (t fa) (t fb))

homListToMaybe :: FunctorMonoidHomomorphism [] Maybe
homListToMaybe = FunctorHom listToMaybe

testPreserveIdentity, testPreserveAppend :: Bool
testPreserveIdentity = preserveIdentity @Product homListToMaybe 'a'
testPreserveAppend = preserveAppend homListToMaybe (Pair ['a'] ['b'])
