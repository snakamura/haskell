{-# LANGUAGE TypeOperators, TypeFamilies, TemplateHaskell, EmptyDataDecls, FlexibleContexts #-}

import Control.Arrow
import Generics.Instant
import Generics.Instant.TH

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

instance Bin a => Bin (Var a) where
    toBin (Var a) = toBin a
    fromBin = fromBin >>> first Var

instance Bin a => Bin (Rec a) where
    toBin (Rec a) = toBin a
    fromBin = fromBin >>> first Rec

instance Bin a => Bin (C c a) where
    toBin (C a) = toBin a
    fromBin = fromBin >>> first C

instance Bin Int where
    toBin n = [3, n]
    fromBin (3:n:ns) = (n, ns)

dft_toBin :: (Representable a, Bin (Rep a)) => a -> [Int]
dft_toBin = toBin . from

dft_fromBin :: (Representable a, Bin (Rep a)) => [Int] -> (a, [Int])
dft_fromBin = fromBin >>> first to

instance Bin a => Bin (Maybe a) where
    toBin = dft_toBin
    fromBin = dft_fromBin

instance Bin a => Bin [a] where
    toBin = dft_toBin
    fromBin = dft_fromBin
