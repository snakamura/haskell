module Array where

import Base

data Cons a b = Cons a b deriving Show
data Nil = Nil deriving Show
infixr `Cons`

instance (Expr a, Expr b) => Expr (Cons a b) where
    eval (Cons a b) = eval a + eval b
instance Expr Nil where
    eval Nil = 0
