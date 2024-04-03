module FunctorProduct where

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

instance (Functor h) => NaturalTransformation (Product h) where
  ntmap ::
    (Functor f, Functor g) =>
    (f ~> g) ->
    (Product h f ~> Product h g)
  ntmap fg (Product ha fa) = Product ha (fg fa)

instance (forall f. NaturalTransformation (Product f)) => BinaturalTransformation Product where
  bintmap ::
    (Functor f, Functor g, Functor h, Functor i) =>
    (f ~> h) ->
    (g ~> i) ->
    (Product f g ~> Product h i)
  bintmap fh gi (Product fa ga) = Product (fh fa) (gi ga)

data Proxy a = Proxy

-- (Hask x Hask, Product, Proxy) is a monoidal category

assoc ::
  (Functor f, Functor g, Functor h) =>
  Product f (Product g h) ~> Product (Product f g) h
assoc (Product fa (Product ga ha)) = Product (Product fa ga) ha

assocInv ::
  (Functor f, Functor g, Functor h) =>
  Product (Product f g) h ~> Product f (Product g h)
assocInv (Product (Product fa ga) ha) = (Product fa (Product ga ha))

left :: (Functor f) => Product Proxy f ~> f
left (Product Proxy fa) = fa

leftInv :: (Functor f) => f ~> Product Proxy f
leftInv fa = Product Proxy fa

right :: (Functor f) => Product f Proxy ~> f
right (Product fa Proxy) = fa

rightInv :: (Functor f) => f ~> Product f Proxy
rightInv fa = Product fa Proxy

instance FunctorMonoid Maybe where
  type Tensor Maybe = Product Maybe Maybe
  type Id Maybe = Proxy

  mu :: Product Maybe Maybe ~> Maybe
  mu (Product (Just a) _) = Just a
  mu (Product _ (Just a)) = Just a
  mu _ = Nothing

  eta :: Proxy ~> Maybe
  eta _ = Nothing
