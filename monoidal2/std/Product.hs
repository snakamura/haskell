module Product where

import Monoid

-- (Hask, (,), ()) is a monoidal category

instance MonoidalCategory (,) () where
  assoc :: (a, (b, c)) -> ((a, b), c)
  assoc (a, (b, c)) = ((a, b), c)

  assocInv :: ((a, b), c) -> (a, (b, c))
  assocInv ((a, b), c) = (a, (b, c))

  left :: ((), a) -> a
  left ((), a) = a

  leftInv :: a -> ((), a)
  leftInv a = ((), a)

  right :: (a, ()) -> a
  right (a, ()) = a

  rightInv :: a -> (a, ())
  rightInv a = (a, ())

instance MonoidObject (,) () Int where
  mu :: (Int, Int) -> Int
  mu (n, m) = n + m

  eta :: () -> Int
  eta () = 0

instance MonoidObject (,) () String where
  mu :: (String, String) -> String
  mu (s1, s2) = s1 ++ s2

  eta :: () -> String
  eta () = ""

instance MonoidObject (,) () (a -> a) where
  mu :: (a -> a, a -> a) -> (a -> a)
  mu (f, g) = g . f

  eta :: () -> (a -> a)
  eta () = id

instance
  {-# OVERLAPPABLE #-}
  (MonoidObject (,) () a) =>
  Semigroup a
  where
  (<>) :: a -> a -> a
  (<>) a1 a2 = mu (a1, a2)

instance
  {-# OVERLAPPABLE #-}
  (MonoidObject (,) () a) =>
  Monoid a
  where
  mempty :: a
  mempty = eta @(,) ()
