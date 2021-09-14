{-# LANGUAGE DataKinds,
             EmptyCase,
             GADTs,
             InstanceSigs,
             KindSignatures,
             RankNTypes,
             ScopedTypeVariables,
             StandaloneDeriving,
             StandaloneKindSignatures,
             TemplateHaskell,
             TypeApplications,
             TypeFamilies,
             TypeOperators,
             UndecidableInstances
#-}

import Data.Kind (Type)
import Data.Singletons.Prelude (FlipSym1)
import Data.Singletons.Sigma (Sigma((:&:)))
import Data.Singletons.TH
import Data.Type.Equality ((:~:)(Refl), testEquality)

singletons [d|
  data Nat = Z | S Nat deriving (Show, Eq)
  |]

type Nat0 = 'Z
type Nat1 = 'S 'Z
type Nat2 = 'S Nat1
type Nat3 = 'S Nat2

type Vec :: Nat -> Type -> Type
data Vec len a where
    VCons :: a -> Vec len a -> Vec ('S len) a
    VNil :: Vec 'Z a
deriving instance Show a => Show (Vec len a)

type SomeVec a = Sigma Nat (FlipSym1 (TyCon Vec) @@ a)

vec :: [a] -> SomeVec a
vec [] = SZ :&: VNil
vec (x:xs) = case vec xs of
               sLen :&: v -> SS sLen :&: VCons x v


sum3 :: Monoid a => Vec Nat3 a -> a
sum3 (VCons a1 (VCons a2 (VCons a3 VNil))) = a1 <> a2 <> a3

trySum3 :: Monoid a => [a] -> Maybe a
trySum3 xs = case vec xs of
               (SS (SS (SS SZ))) :&: v -> Just $ sum3 v
               _ -> Nothing


vzip :: Vec len a -> Vec len b -> Vec len (a, b)
vzip VNil VNil = VNil
vzip (VCons x xs) (VCons y ys) = VCons (x, y) $ vzip xs ys

tryZip :: [a] -> [b] -> Maybe (SomeVec (a, b))
tryZip xs ys = case (vec xs, vec ys) of
                 (sLenX :&: vx, sLenY :&: vy) ->
                   case testEquality sLenX sLenY of
                     Just Refl -> Just (sLenX :&: vzip vx vy)
                     Nothing -> Nothing
