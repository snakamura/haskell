{-# LANGUAGE GADTs,
             NoImplicitPrelude #-}

data Z
data S n

data List len a where
    Nil :: List Z a
    Cons :: a -> List l a -> List (S l) a

head :: List (S l) a -> a
head (Cons a _) = a

tail :: List (S l) a -> List l a
tail (Cons _ r) = r
