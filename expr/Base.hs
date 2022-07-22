module Base where

class Expr a where
    eval :: a -> Int

newtype Literal = Literal Int deriving Show

instance Expr Literal where
    eval (Literal n) = n
