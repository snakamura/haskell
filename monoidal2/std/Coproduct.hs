module Coproduct where

import Data.Char
import Data.Void
import Monoid

-- (Hask, Either, Void) is a monoidal category

instance MonoidalCategory Either where
  type Unit Either = Void

  assoc :: Either a (Either b c) -> Either (Either a b) c
  assoc (Left a) = Left (Left a)
  assoc (Right (Left b)) = Left (Right b)
  assoc (Right (Right c)) = Right c

  assocInv :: Either (Either a b) c -> Either a (Either b c)
  assocInv (Left (Left a)) = Left a
  assocInv (Left (Right b)) = Right (Left b)
  assocInv (Right c) = Right (Right c)

  left :: Either Void a -> a
  left (Left v) = absurd v
  left (Right a) = a

  leftInv :: a -> Either Void a
  leftInv = Right

  right :: Either a Void -> a
  right (Left a) = a
  right (Right v) = absurd v

  rightInv :: a -> Either a Void
  rightInv = Left

instance MonoidObject Either a where
  mu :: Either a a -> a
  mu (Left a) = a
  mu (Right a) = a

  eta :: Void -> a
  eta = absurd

instance MonoidObject Either (MonoidHomomorphism a b) where
  mu :: Either (MonoidHomomorphism a b) (MonoidHomomorphism a b) -> MonoidHomomorphism a b
  mu (Left (Hom f)) = Hom $ \a -> mu (Left (f a))
  mu (Right (Hom g)) = Hom $ \a -> mu (Right (g a))

  eta :: Void -> MonoidHomomorphism a b
  eta = absurd

instance
  ( MonoidObject Either m1,
    MonoidObject Either m2,
    Eq m2
  ) =>
  MonoidHomomorphismLaws Either m1 m2
  where
  preserveIdentity :: MonoidHomomorphism m1 m2 -> Bool
  preserveIdentity (Hom _) = True

  -- preserveIdentity (Hom f) = f (eta @Either undefined) == eta @Either undefined

  preserveAppend :: MonoidHomomorphism m1 m2 -> Either m1 m1 -> Bool
  preserveAppend (Hom f) e@(Left a) = f (mu e) == mu (Left (f a))
  preserveAppend (Hom f) e@(Right b) = f (mu e) == mu (Right (f b))

homOrd :: MonoidHomomorphism Char Int
homOrd = Hom ord

testPreserveIdentity, testPreserveAppend :: Bool
testPreserveIdentity = preserveIdentity @Either homOrd
testPreserveAppend = preserveAppend homOrd (Left 'A')
