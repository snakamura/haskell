{-# LANGUAGE DataKinds,
             GADTs,
             KindSignatures,
             NoImplicitPrelude,
             TypeOperators #-}

import GHC.TypeLits (Nat, type(+), type(<=))

data List (len :: Nat) a where
    Nil :: List 0 a
    Cons :: a -> List l a -> List (l + 1) a

head :: (1 <= len) => List len a -> a
head (Cons a _) = a
