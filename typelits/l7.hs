{-# LANGUAGE DataKinds,
             GADTs,
             KindSignatures,
             ScopedTypeVariables,
             StandaloneDeriving,
             TypeOperators #-}

import Data.Maybe (isJust)
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (KnownNat, Nat, SomeNat(SomeNat), type(+), natVal, sameNat, someNatVal)

data List (len :: Nat) a where
    Nil :: List 0 a
    Cons :: a -> List l a -> List (l + 1) a

deriving instance Show a => Show (List len a)

length :: forall len a. KnownNat len => List len a -> Integer
length _ = natVal (Proxy :: Proxy len)

isLength :: forall len a. KnownNat len => Integer -> List len a -> Bool
isLength n _ | Just (SomeNat p) <- someNatVal n = isJust (sameNat (Proxy :: Proxy len) p)
             | otherwise = False
