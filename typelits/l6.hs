{-# LANGUAGE DataKinds,
             GADTs,
             KindSignatures,
             NoImplicitPrelude,
             StandaloneDeriving,
             TypeOperators #-}

import GHC.TypeLits (Nat, type(+), type(-), type(<=))
import Prelude (Show)
import Unsafe.Coerce (unsafeCoerce)

data List (len :: Nat) a where
    Nil :: List 0 a
    Cons :: a -> List l a -> List (l + 1) a

deriving instance Show a => Show (List len a)

head :: (1 <= len) => List len a -> a
head (Cons a _) = a

tail :: (1 <= len) => List len a -> List (len - 1) a
tail (Cons _ r) = unsafeCoerce r

append :: List len1 a -> List len2 a -> List (len1 + len2) a
append Nil l = l
append (Cons a r) l = unsafeCoerce (Cons a (append r l))
