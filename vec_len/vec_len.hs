{-# LANGUAGE DataKinds,
             GADTs,
             RankNTypes,
             StandaloneDeriving,
             StandaloneKindSignatures,
             TypeOperators
#-}

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))

data Nat = Z | S Nat

type Nat0 = 'Z
type Nat1 = 'S 'Z
type Nat2 = 'S Nat1
type Nat3 = 'S Nat2

type SNat :: Nat -> Type
data SNat n where
    SZ :: SNat 'Z
    SS :: SNat m -> SNat ('S m)
deriving instance Show (SNat n)

sameSNat :: SNat n -> SNat m -> Maybe (n :~: m)
sameSNat SZ SZ = Just Refl
sameSNat (SS sNat1) (SS sNat2) = case sameSNat sNat1 sNat2 of
                                   Just Refl -> Just Refl
                                   Nothing -> Nothing
sameSNat _ _ = Nothing

type Vec :: Nat -> Type -> Type
data Vec len a where
    VCons :: a -> Vec len a -> Vec ('S len) a
    VNil :: Vec 'Z a
deriving instance Show a => Show (Vec len a)

data SomeVec a = forall len. SomeVec (SNat len) (Vec len a)
deriving instance Show a => Show (SomeVec a)

vec :: [a] -> SomeVec a
vec [] = SomeVec SZ VNil
vec (x:xs) = case vec xs of
               SomeVec sLen v -> SomeVec (SS sLen) (VCons x v)


sum3 :: Monoid a => Vec Nat3 a -> a
sum3 (VCons a1 (VCons a2 (VCons a3 VNil))) = a1 <> a2 <> a3

trySum3 :: Monoid a => [a] -> Maybe a
trySum3 xs = case vec xs of
               SomeVec (SS (SS (SS SZ))) v -> Just $ sum3 v
               _ -> Nothing


vzip :: Vec len a -> Vec len b -> Vec len (a, b)
vzip VNil VNil = VNil
vzip (VCons x xs) (VCons y ys) = VCons (x, y) $ vzip xs ys

tryZip :: [a] -> [b] -> Maybe (SomeVec (a, b))
tryZip xs ys = case (vec xs, vec ys) of
                 (SomeVec sLenX vx, SomeVec sLenY vy) ->
                   case sameSNat sLenX sLenY of
                     Just Refl -> Just (SomeVec sLenX (vzip vx vy))
                     Nothing -> Nothing
