{-# LANGUAGE Generics, TypeOperators #-}

import Data.Generics

class X a where
    x :: a -> String
    x {| Unit    |} _         = "U"
    x {| p :*: q |} (a :*: b) = x a ++ "*" ++ x b
    x {| p :+: q |} (Inl a)   = "L" ++ x a
    x {| p :+: q |} (Inr a)   = "R" ++ x a


instance X Int where
    x n = show n


data S a = S a

instance X a => X (S a)


data T a b = T a b

instance (X a, X b) => X (T a b)


data U = U

instance X U


data V = V0
       | V1 Int
       | V2 Int Int

instance X V
