{-# LANGUAGE DataKinds,
             GADTs,
             KindSignatures,
             NoImplicitPrelude,
             StandaloneDeriving,
             TypeFamilies #-}

import Prelude (Show)

data Num = Z | S Num

data List (len :: Num) a where
    Nil :: List Z a
    Cons :: a -> List l a -> List (S l) a

deriving instance Show a => Show (List len a)

type family Plus (a :: Num) (b :: Num) :: Num where
    Plus Z b = b
    Plus (S a) b = S (Plus a b)

head :: List (S l) a -> a
head (Cons a _) = a

tail :: List (S l) a -> List l a
tail (Cons _ r) = r

append :: List l1 a -> List l2 a -> List (Plus l1 l2) a
append Nil l = l
append (Cons a r) l = Cons a (append r l)
