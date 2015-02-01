{-# LANGUAGE DataKinds,
             GADTs,
             NoImplicitPrelude,
             StandaloneDeriving #-}

import Prelude (Show)

data Num = Z | S Num

data List len a where
    Nil :: List Z a
    Cons :: a -> List l a -> List (S l) a

deriving instance Show a => Show (List len a)

head :: List (S l) a -> a
head (Cons a _) = a

tail :: List (S l) a -> List l a
tail (Cons _ r) = r
