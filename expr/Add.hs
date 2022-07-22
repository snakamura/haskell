module Add where

import Base

data Add a b = Add a b deriving Show

instance (Expr a, Expr b) => Expr (Add a b) where
    eval (Add a b) = eval a + eval b
