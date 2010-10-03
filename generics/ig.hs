{-# LANGUAGE TypeOperators, TypeFamilies, TemplateHaskell, EmptyDataDecls, FlexibleContexts #-}

import Generics.Instant
import Generics.Instant.TH

class X a where
    x :: a -> String

instance X U where
    x U = "U"

instance (X a, X b) => X (a :*: b) where
    x (a :*: b) = "(" ++ x a ++ "*" ++ x b ++ ")"

instance (X a, X b) => X (a :+: b) where
    x (L a) = "L(" ++ x a ++ ")"
    x (R a) = "R(" ++ x a ++ ")"

instance X a => X (Var a) where
    x (Var a) = "Var(" ++ x a ++ ")"

instance X a => X (Rec a) where
    x (Rec a) = "Rec(" ++ x a ++ ")"

instance (X a, Constructor c) => X (C c a) where
    x c@(C a) = conName c ++ "(" ++ x a ++ ")"


instance X Int where
    x n = show n

dft_x :: (Representable a, X (Rep a)) => a -> String
dft_x = x . from


data K a = K a

deriveAll ''K

instance X a => X (K a) where
    x = dft_x


data K2 a b = K2 a b

deriveAll ''K2

instance (X a, X b) => X (K2 a b) where
    x = dft_x


data K3 a = K31
          | K32 a

deriveAll ''K3

instance X a => X (K3 a) where
    x = dft_x
