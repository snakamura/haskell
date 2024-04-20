module Product where

import Data.Kind
import Data.Monoid qualified
import Data.Semigroup qualified
import Functor
import Monoid
import Prelude (Int, Num (..), String, id, (++), (.))

type Product :: Type -> Type -> Type
data Product a b = Product a b

instance Functor (Product p) where
  fmap :: (a -> b) -> (Product p a -> Product p b)
  fmap ab (Product p a) = Product p (ab a)

instance Bifunctor Product where
  bimap :: (a -> c) -> (b -> d) -> (Product a b -> Product c d)
  bimap ac bd (Product a b) = Product (ac a) (bd b)

-- (Hask, Product, ()) is a monoidal category

instance MonoidalCategory Product where
  type Unit Product = ()

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

instance MonoidObject Int where
  type Tensor Int = Product

  mu :: Product Int Int -> Int
  mu (Product n m) = n + m

  eta :: () -> Int
  eta () = 0

instance MonoidObject String where
  type Tensor String = Product

  mu :: Product String String -> String
  mu (Product s1 s2) = s1 ++ s2

  eta :: () -> String
  eta () = ""

instance MonoidObject (a -> a) where
  type Tensor (a -> a) = Product

  mu :: Product (a -> a) (a -> a) -> (a -> a)
  mu (Product f g) = g . f

  eta :: () -> (a -> a)
  eta () = id

instance
  ( MonoidObject a,
    Tensor a ~ Product,
    Unit (Tensor a) ~ ()
  ) =>
  Data.Semigroup.Semigroup a
  where
  (<>) :: a -> a -> a
  (<>) a1 a2 = mu (Product a1 a2)

instance
  ( MonoidObject a,
    Tensor a ~ Product,
    Unit (Tensor a) ~ ()
  ) =>
  Data.Monoid.Monoid a
  where
  mempty :: a
  mempty = eta ()
