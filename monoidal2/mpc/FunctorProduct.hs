module FunctorProduct where

import Control.Applicative qualified
import Data.Functor qualified
import Data.Maybe
import Functor
import FunctorMonoid
import NaturalTransformation
import Prelude ()

type Product :: FunctorType -> FunctorType -> FunctorType
data Product f g a = Product (f a) (g a)

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap :: (a -> b) -> (Product f g a -> Product f g b)
  fmap ab (Product fa ga) = Product (fmap ab fa) (fmap ab ga)

instance (Functor f) => NaturalTransformation (Product f) where
  ntmap ::
    (Functor g, Functor h) =>
    (g ~> h) ->
    (Product f g ~> Product f h)
  ntmap gh (Product fa ga) = Product fa (gh ga)

instance
  (forall f. (Functor f) => NaturalTransformation (Product f)) =>
  BinaturalTransformation Product
  where
  bintmap ::
    (Functor f, Functor g, Functor h, Functor i) =>
    (f ~> h) ->
    (g ~> i) ->
    (Product f g ~> Product h i)
  bintmap fh gi (Product fa ga) = Product (fh fa) (gi ga)

data Proxy a = Proxy

instance Functor Proxy where
  fmap :: (a -> b) -> (Proxy a -> Proxy b)
  fmap _ Proxy = Proxy

-- (Hask x Hask, Product, Proxy) is a monoidal category

instance FunctorMonoidalCategory Product Proxy where
  assoc ::
    (Functor f, Functor g, Functor h) =>
    Product f (Product g h)
      ~> Product
           (Product f g)
           h
  assoc (Product fa (Product ga ha)) = Product (Product fa ga) ha

  assocInv ::
    (Functor f, Functor g, Functor h) =>
    Product (Product f g) h
      ~> Product
           f
           (Product g h)
  assocInv (Product (Product fa ga) ha) = (Product fa (Product ga ha))

  left :: (Functor f) => Product Proxy f ~> f
  left (Product Proxy fa) = fa

  leftInv :: (Functor f) => f ~> Product Proxy f
  leftInv fa = Product Proxy fa

  right :: (Functor f) => Product f Proxy ~> f
  right (Product fa Proxy) = fa

  rightInv :: (Functor f) => f ~> Product f Proxy
  rightInv fa = Product fa Proxy

instance Functor Maybe where
  fmap :: (a -> b) -> (Maybe a -> Maybe b)
  fmap ab (Just a) = Just (ab a)
  fmap _ Nothing = Nothing

instance FunctorMonoidObject Product Proxy Maybe where
  mu :: Product Maybe Maybe ~> Maybe
  mu (Product (Just a) _) = Just a
  mu (Product _ (Just a)) = Just a
  mu _ = Nothing

  eta :: Proxy ~> Maybe
  eta _ = Nothing

instance
  {-# OVERLAPPABLE #-}
  ( Data.Functor.Functor f,
    Control.Applicative.Applicative f,
    FunctorMonoidObject Product Proxy f
  ) =>
  Control.Applicative.Alternative f
  where
  (<|>) :: f a -> f a -> f a
  (<|>) fa1 fa2 = mu (Product fa1 fa2)

  empty :: f a
  empty = eta @Product Proxy
