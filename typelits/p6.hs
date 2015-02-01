{-# LANGUAGE ConstraintKinds,
             GADTs,
             NoImplicitPrelude,
             StandaloneDeriving,
             TypeFamilies #-}

import Prelude (Show)

data Z
data S n

data List len a where
    Nil :: List Z a
    Cons :: a -> List l a -> List (S l) a

deriving instance Show a => Show (List len a)

data True
data False

type family NotZ a where
    NotZ Z = False
    NotZ (S n) = True

type NotZero a = NotZ a ~ True

head :: NotZero l => List l a -> a
head (Cons a _) = a
