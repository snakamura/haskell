{-# LANGUAGE FlexibleInstances, InstanceSigs, MultiParamTypeClasses #-}

import Data.Monoid

class (Monoid m1, Monoid m2) => MonoidHomomorphism m1 m2 where
  hom :: m1 -> m2

instance MonoidHomomorphism [a] (Sum Int) where
  hom :: [a] -> Sum Int
  hom = Sum . length
