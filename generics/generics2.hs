{-# LANGUAGE Generics, TypeOperators #-}

import Control.Arrow
import Data.Generics

class Bin a where
    toBin   :: a -> [Int]
    toBin {| Unit    |} Unit      = []
    toBin {| a :+: b |} (Inl x)   = 0 : toBin x
    toBin {| a :+: b |} (Inr y)   = 1 : toBin y
    toBin {| a :*: b |} (x :*: y) = toBin x ++ toBin y
    
    fromBin :: [Int] -> (a, [Int])
    fromBin {| Unit    |} bs      = (Unit, bs)
    fromBin {| a :+: b |} (0:bs)  = first Inl (fromBin bs)
    fromBin {| a :+: b |} (1:bs)  = first Inr (fromBin bs)
    fromBin {| a :*: b |}         = fromBin >>> second fromBin >>> uncurry (first . (:*:))


instance Bin Int where
    toBin n = [3, n]
    fromBin (3:n:ns) = (n, ns)

instance Bin a => Bin (Maybe a)

instance Bin a => Bin [a]

newtype X = X Int deriving Show

instance Bin X

data Y = Y1
       | Y2
  deriving Show

instance Bin Y
