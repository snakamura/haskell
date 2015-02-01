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

type family NotEmpty' (list :: Num -> * -> *) (l :: Num) :: Bool where
    NotEmpty' list Z = False
    NotEmpty' list (S n) = True

type NotEmpty list len = NotEmpty' list len ~ True

head :: NotEmpty List l => List l a -> a
head (Cons a _) = a
