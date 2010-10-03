{-# LANGUAGE TypeOperators, TypeFamilies #-}

data U = U
data a :*: b = a :*: b
data a :+: b = L a
             | R b
data V a = V a

class R a where
    type S a
    from :: a -> S a
    to :: S a -> a

instance R U where
    type S U = U
    from = id
    to = id

instance (R a, R b) => R (a :*: b) where
    type S (a :*: b) = a :*: b
    from = id
    to = id

instance (R a, R b) => R (a :+: b) where
    type S (a :+: b) = a :+: b
    from = id
    to = id

instance R (V a) where
    type S (V a) = V a
    from = id
    to = id

instance R Int where
    type S Int = Int
    from = id
    to = id


class X a where
    x :: a -> String

instance X U where
    x U = "U"

instance (X a, X b) => X (a :*: b) where
    x (a :*: b) = "(" ++ x a ++ "*" ++ x b ++ ")"

instance (X a, X b) => X (a :+: b) where
    x (L a) = "L(" ++ x a ++ ")"
    x (R a) = "R(" ++ x a ++ ")"

instance X a => X (V a) where
    x (V a) = "V(" ++ x a ++ ")"


instance X Int where
    x n = show n

data K a = K a

instance R a => R (K a) where
    type S (K a) = V a
    from (K a) = V a
    to (V a) = K a

instance (R a, X a) => X (K a) where
    x = x . from

data K2 a b = K2 a b

instance R (K2 a b) where
    type S (K2 a b) = V a :*: V b
    from (K2 a b) = V a :*: V b
    to (V a :*: V b) = K2 a b

instance (X a, X b) => X (K2 a b) where
    x = x . from

data K3 a = K31
          | K32 a

instance R (K3 a) where
    type S (K3 a) = U :+: (V a)
    from K31 = L U
    from (K32 a) = R (V a)
    to (L U) = K31
    to (R (V a)) = K32 a

instance X a => X (K3 a) where
    x = x . from
