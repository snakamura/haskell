{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Data.Monoid

class (Monoid m1, Monoid m2) => MonoidHomomorphism m1 m2 where
  hom :: m1 -> m2

instance MonoidHomomorphism [a] (Sum Int) where
  hom :: [a] -> Sum Int
  hom = Sum . length

preserveIdentity :: forall m1 m2. (MonoidHomomorphism m1 m2, Eq m2) => Bool
preserveIdentity = hom (mempty @m1) == mempty @m2

preserveAppend :: forall m1 m2. (MonoidHomomorphism m1 m2, Eq m2) => m1 -> m1 -> Bool
preserveAppend a b = hom @m1 @m2 (a <> b) == hom a <> hom b

testPreserveIdentity, testPreserveAppend :: Bool
testPreserveIdentity = preserveIdentity @[Char] @(Sum Int)
testPreserveAppend = preserveAppend @[Char] @(Sum Int) ['A', 'B'] ['C', 'D', 'E']
