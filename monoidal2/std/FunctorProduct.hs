module FunctorProduct where

import Control.Applicative
import Data.Functor.Product
import Data.Proxy
import FunctorMonoid
import NaturalTransformation

instance (Functor f) => NaturalTransformation (Product f) where
  ntmap ::
    (Functor g, Functor h) =>
    (g ~> h) ->
    (Product f g ~> Product f h)
  ntmap gh (Pair fa ga) = Pair fa (gh ga)

instance
  (forall f. (Functor f) => NaturalTransformation (Product f)) =>
  BinaturalTransformation Product
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
