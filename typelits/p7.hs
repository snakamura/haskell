{-# LANGUAGE ConstraintKinds,
             DataKinds,
             GADTs,
             KindSignatures,
             NoImplicitPrelude,
             StandaloneDeriving,
             TypeFamilies #-}

import Prelude (Bool(True, False), Show)

data Num = Z | S Num

data List len a where
    Nil :: List Z a
    Cons :: a -> List l a -> List (S l) a

deriving instance Show a => Show (List len a)

type family NotZ (len :: Num) :: Bool where
    NotZ Z = False
    NotZ (S n) = True

type NotZero a = NotZ a ~ True

head :: NotZero l => List l a -> a
head (Cons a _) = a
