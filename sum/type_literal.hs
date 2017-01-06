{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies, TypeOperators, UndecidableInstances #-}

import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (Nat, type (<=?), type (+), type (-), natVal)

type family Sum (n :: Nat) :: Nat where
    Sum n = Sum' n (n <=? 0)

type family Sum' (n :: Nat) (o :: Bool) :: Nat where
    Sum' n True = 0
    Sum' n False = n + Sum (n - 1)


sum5 :: Integer
sum5 = natVal (Proxy :: Proxy (Sum 5))
