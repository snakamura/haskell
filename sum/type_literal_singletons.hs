{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies, TypeOperators, UndecidableInstances #-}

import Data.Singletons.Prelude (Sing, fromSing, sing)
import GHC.TypeLits (Nat, type (<=?), type (+), type (-))

type family Sum (n :: Nat) :: Nat where
    Sum n = Sum' n (n <=? 0)

type family Sum' (n :: Nat) (o :: Bool) :: Nat where
    Sum' n True = 0
    Sum' n False = n + Sum (n - 1)


sum5 :: Integer
sum5 = fromSing (sing :: Sing (Sum 5))
