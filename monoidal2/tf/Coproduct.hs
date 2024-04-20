module Coproduct where

import Data.Kind
import Functor
import Monoid
import Prelude ()

type Coproduct :: Type -> Type -> Type
data Coproduct a b = Left a | Right b

instance Functor (Coproduct p) where
  fmap :: (a -> b) -> (Coproduct p a -> Coproduct p b)
  fmap _ (Left b) = Left b
  fmap ab (Right a) = Right (ab a)

instance Bifunctor Coproduct where
  bimap :: (a -> c) -> (b -> d) -> (Coproduct a b -> Coproduct c d)
  bimap ac _ (Left a) = Left (ac a)
  bimap _ bd (Right b) = Right (bd b)

data Void

absurd :: Void -> a
absurd = absurd

-- (Hask, Coproduct, Void) is a monoidal category

instance MonoidalCategory Coproduct where
  type Unit Coproduct = Void

  assoc :: Coproduct a (Coproduct b c) -> Coproduct (Coproduct a b) c
  assoc (Left a) = Left (Left a)
  assoc (Right (Left b)) = Left (Right b)
  assoc (Right (Right c)) = Right c

  assocInv :: Coproduct (Coproduct a b) c -> Coproduct a (Coproduct b c)
  assocInv (Left (Left a)) = Left a
  assocInv (Left (Right b)) = Right (Left b)
  assocInv (Right c) = Right (Right c)

  left :: Coproduct Void a -> a
  left (Left v) = absurd v
  left (Right a) = a

  leftInv :: a -> Coproduct Void a
  leftInv = Right

  right :: Coproduct a Void -> a
  right (Left a) = a
  right (Right v) = absurd v

  rightInv :: a -> Coproduct a Void
  rightInv = Left

instance MonoidObject a where
  type Tensor a = Coproduct

  mu :: Coproduct a a -> a
  mu (Left a) = a
  mu (Right a) = a

  eta :: Void -> a
  eta = absurd
