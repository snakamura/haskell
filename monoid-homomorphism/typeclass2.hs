{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Data.Monoid

class (Monoid m1, Monoid m2, Eq m2) => MonoidHomomorphism m1 m2 where
  hom :: m1 -> m2

  preserveIdentity :: Bool
  preserveIdentity = hom (mempty @m1) == mempty @m2

  preserveAppend :: m1 -> m1 -> Bool
  preserveAppend a b = hom @m1 @m2 (a <> b) == hom a <> hom b

instance MonoidHomomorphism [a] (Sum Int) where
  hom :: [a] -> Sum Int
  hom = Sum . length

testPreserveIdentity, testPreserveAppend :: Bool
testPreserveIdentity = preserveIdentity @[Char] @(Sum Int)
testPreserveAppend = preserveAppend @[Char] @(Sum Int) ['A', 'B'] ['C', 'D', 'E']