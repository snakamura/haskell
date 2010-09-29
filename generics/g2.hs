{-# LANGUAGE TypeOperators, TypeFamilies #-}

import Control.Arrow

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


class Bin a where
    toBin   :: a -> [Int]
    fromBin :: [Int] -> (a, [Int])

instance Bin U where
    toBin U = []
    fromBin bs = (U, bs)

instance (Bin a, Bin b) => Bin (a :*: b) where
    toBin (x :*: y) = toBin x ++ toBin y
    fromBin = fromBin >>> second fromBin >>> uncurry (first . (:*:))

instance (Bin a, Bin b) => Bin (a :+: b) where
    toBin (L x) = 0:toBin x
    toBin (R y) = 1:toBin y
    fromBin (0:bs) = first L (fromBin bs)
    fromBin (1:bs) = first R (fromBin bs)

instance Bin a => Bin (V a) where
    toBin (V a) = toBin a
    fromBin = fromBin >>> first V

instance Bin Int where
    toBin n = [3, n]
    fromBin (3:n:ns) = (n, ns)


instance R a => R (Maybe a) where
    type S (Maybe a) = U :+: V a
    from Nothing = L U
    from (Just x) = R (V x)
    to (L U) = Nothing
    to (R (V x)) = Just x

instance (R a, Bin a) => Bin (Maybe a) where
    toBin = toBin . from
    fromBin = fromBin >>> first to


instance R a => R [a] where
    type S [a] = U :+: (V a :*: V [a])
    from [] = L U
    from (x:xs) = R (V x :*: V xs)
    to (L U) = []
    to (R (V x :*: V xs)) = x:xs

instance (R a, Bin a) => Bin [a] where
    toBin = toBin . from
    fromBin = fromBin >>> first to
