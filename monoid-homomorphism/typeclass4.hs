{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE RequiredTypeArguments #-}


import Data.Monoid

class (Monoid m1, Monoid m2) => MonoidHomomorphism m1 m2 where
  hom :: m1 -> m2

instance MonoidHomomorphism [a] (Sum Int) where
  hom :: [a] -> Sum Int
  hom = Sum . length

preserveIdentity ::
  forall m1 ->
  forall m2 ->
  (MonoidHomomorphism m1 m2, Eq m2) =>
  Bool
preserveIdentity m1 m2 = hom (mempty @m1) == mempty @m2

preserveAppend ::
  m1 ->
  m1 ->
  forall m2 ->
  (MonoidHomomorphism m1 m2, Eq m2) =>
  Bool
preserveAppend a b m2 = hom @_ @m2 (a <> b) == hom a <> hom b

testPreserveIdentity, testPreserveAppend :: Bool
testPreserveIdentity = preserveIdentity (type [Char]) (type (Sum Int))
testPreserveAppend = preserveAppend ['A', 'B'] ['C', 'D', 'E'] (type (Sum Int))
