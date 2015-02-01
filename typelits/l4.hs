{-# LANGUAGE ConstraintKinds,
             DataKinds,
             GADTs,
             KindSignatures,
             NoImplicitPrelude,
             StandaloneDeriving,
             TypeFamilies,
             TypeOperators #-}

import GHC.TypeLits (CmpNat, Nat, type(+))
import Prelude (Bool(True, False), Ordering(LT, EQ, GT), Show)

data List (len :: Nat) a where
    Nil :: List 0 a
    Cons :: a -> List l a -> List (l + 1) a

deriving instance Show a => Show (List len a)

type Greater a b = CmpNat a b ~ GT

head :: Greater len 0 => List len a -> a
head (Cons a _) = a
