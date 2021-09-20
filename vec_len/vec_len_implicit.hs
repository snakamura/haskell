{-# LANGUAGE DataKinds,
             FlexibleInstances,
             GADTs,
             KindSignatures,
             RankNTypes,
             ScopedTypeVariables,
             StandaloneDeriving,
             StandaloneKindSignatures,
             TypeApplications,
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

class KnownNat (n :: Nat) where
    natVal :: SNat n
instance KnownNat 'Z where
    natVal = SZ
instance KnownNat n => KnownNat ('S n) where
    natVal = SS natVal


sameSNat :: forall n m. (KnownNat n, KnownNat m) => Maybe (n :~: m)
sameSNat = sameSNat' (natVal @n) (natVal @m)
  where
    sameSNat' :: SNat p -> SNat q -> Maybe (p :~: q)
    sameSNat' SZ SZ = Just Refl
    sameSNat' (SS sNat1) (SS sNat2) = case sameSNat' sNat1 sNat2 of
                                        Just Refl -> Just Refl
                                        Nothing -> Nothing
    sameSNat' _ _ = Nothing

type Vec :: Nat -> Type -> Type
data Vec len a where
    VCons :: a -> Vec len a -> Vec ('S len) a
    VNil :: Vec 'Z a
deriving instance Show a => Show (Vec len a)

data SomeVec a = forall len. KnownNat len => SomeVec (Vec len a)
deriving instance Show a => Show (SomeVec a)

vec :: [a] -> SomeVec a
vec [] = SomeVec VNil
vec (x:xs) = case vec xs of
               SomeVec v -> SomeVec (VCons x v)


sum3 :: Monoid a => Vec Nat3 a -> a
sum3 (VCons a1 (VCons a2 (VCons a3 VNil))) = a1 <> a2 <> a3

trySum3 :: Monoid a => [a] -> Maybe a
trySum3 xs = case vec xs of
               SomeVec (v :: Vec len a) ->
                 case natVal @len of
                   SS (SS (SS SZ)) -> Just $ sum3 v
                   _ -> Nothing


vzip :: Vec len a -> Vec len b -> Vec len (a, b)
vzip VNil VNil = VNil
vzip (VCons x xs) (VCons y ys) = VCons (x, y) $ vzip xs ys

tryZip :: [a] -> [b] -> Maybe (SomeVec (a, b))
tryZip xs ys = case (vec xs, vec ys) of
                 (SomeVec (vx :: Vec len1 a), SomeVec (vy :: Vec len2 b)) ->
                   case sameSNat @len1 @len2 of
                     Just Refl -> Just (SomeVec (vzip vx vy))
                     Nothing -> Nothing
