{-# LANGUAGE TypeAbstractions #-}

import Data.Monoid

newtype MonoidHomomorphism m1 m2 = Hom (m1 -> m2)

preserveIdentity ::
  (Monoid m1, Monoid m2, Eq m2) =>
  MonoidHomomorphism m1 m2 ->
  Bool
preserveIdentity @m1 @m2 (Hom f) = f (mempty @m1) == mempty @m2

preserveAppend ::
  (Monoid m1, Monoid m2, Eq m2) =>
  MonoidHomomorphism m1 m2 ->
  m1 ->
  m1 ->
  Bool
preserveAppend (Hom f) a b = f (a <> b) == f a <> f b

hom :: MonoidHomomorphism [a] (Sum Int)
hom = Hom (Sum . length)

testPreserveIdentity, testPreserveAppend :: Bool
testPreserveIdentity = preserveIdentity hom
testPreserveAppend = preserveAppend hom ['A', 'B'] ['C', 'D', 'E']

instance Semigroup b => Semigroup (MonoidHomomorphism a b) where
  (Hom f) <> (Hom g) = Hom $ \a -> f a <> g a

instance Monoid b => Monoid (MonoidHomomorphism a b) where
  mempty = Hom (const mempty)
