module Product where

import Data.Kind
import Data.Monoid qualified
import Data.Semigroup qualified
import Functor
import Monoid
import Prelude (Int, Num (..), String, (++))

type Product :: Type -> Type -> Type
data Product a b = Product a b

instance Functor (Product p) where
  fmap :: (a -> b) -> (Product p a -> Product p b)
  fmap ab (Product p a) = Product p (ab a)

instance Bifunctor Product where
  bimap :: (a -> c) -> (b -> d) -> (Product a b -> Product c d)
  bimap ac bd (Product a b) = Product (ac a) (bd b)

-- (Hask, Product, ()) is a monoidal category

assoc :: Product a (Product b c) -> Product (Product a b) c
assoc (Product a (Product b c)) = Product (Product a b) c

assocInv :: Product (Product a b) c -> Product a (Product b c)
assocInv (Product (Product a b) c) = Product a (Product b c)

left :: Product () a -> a
left (Product () a) = a

leftInv :: a -> Product () a
leftInv a = Product () a

right :: Product a () -> a
right (Product a ()) = a

rightInv :: a -> Product a ()
rightInv a = Product a ()

instance Monoid Int where
  type Tensor Int = Product Int Int
  type Id Int = ()

  mu :: Product Int Int -> Int
  mu (Product n m) = n + m

  eta :: () -> Int
  eta () = 0

instance Monoid String where
  type Tensor String = Product String String
  type Id String = ()

  mu :: Product String String -> String
  mu (Product s1 s2) = s1 ++ s2

  eta :: () -> String
  eta () = ""

instance
  ( Monoid a,
    Tensor a ~ Product a a,
    Id a ~ ()
  ) =>
  Data.Semigroup.Semigroup a
  where
  (<>) :: a -> a -> a
  (<>) a1 a2 = mu (Product a1 a2)

instance
  ( Monoid a,
    Tensor a ~ Product a a,
    Id a ~ ()
  ) =>
  Data.Monoid.Monoid a
  where
  mempty :: a
  mempty = eta ()
