module Product where

import Data.Kind

type data J = J1 | J2

type Delta :: Type -> (J -> Type)
newtype Delta v a = Delta v

type Lim :: (J -> Type) -> Type
data Lim f = Lim (f J1) (f J2)

{-
homDeltaVtoF ::
  forall v f.
  (Cone v (f J1) (f J2)) =>
  (Delta v J1 -> f J1, Delta v J2 -> f J2)
homDeltaVtoF =
  ( \(Delta v) -> p @_ @(f J1) @(f J2) v,
    \(Delta v) -> q @_ @(f J1) @(f J2) v
  )

homVtoLimV :: (Cone v (F J1) (F J2)) => v -> Lim F
homVtoLimV v = Lim (p @_ @(F J1) @(F J2) v) (q @_ @(F J1) @(F J2) v)
-}

type Adjunction :: (Type -> (J -> Type)) -> ((J -> Type) -> Type) -> Constraint
class Adjunction f g | f -> g, g -> f where
  leftAdjunct ::
    forall (v :: Type) (d :: J -> Type) (j :: J).
    (f v j -> d j) -> (v -> g d)
  rightAdjunct ::
    forall (v :: Type) (d :: J -> Type) (j :: J).
    (v -> g d) -> (f v j -> d j)

type Adjunction' :: (Type -> (J -> Type)) -> ((J -> Type) -> Type) -> Constraint
class Adjunction' f g | f -> g, g -> f where
  leftAdjunct' ::
    forall (v :: Type) (d :: J -> Type).
    (f v J1 -> d J1, f v J2 -> d J2) -> (v -> g d)
  rightAdjunct' ::
    forall (v :: Type) (d :: J -> Type).
    (v -> g d) -> (f v J1 -> d J1, f v J2 -> d J2)

instance Adjunction' Delta Lim where
  leftAdjunct' ::
    forall (v :: Type) (d :: J -> Type).
    (Delta v J1 -> d J1, Delta v J2 -> d J2) -> (v -> Lim d)
  leftAdjunct' (p, q) = \v -> Lim (p (Delta v)) (q (Delta v))

  rightAdjunct' ::
    forall (v :: Type) (d :: J -> Type).
    (v -> Lim d) -> (Delta v J1 -> d J1, Delta v J2 -> d J2)
  rightAdjunct' f =
    ( \(Delta v) -> let Lim a _ = f v in a,
      \(Delta v) -> let Lim _ b = f v in b
    )

type D :: J -> Type
data family D j
data instance D J1
data instance D J2

data C

data V = V (D J1) (D J2) C

hom1 :: (Delta V J1 -> (D J1, D J2), Delta V J2 -> (D J1, D J2))
hom1 = (\(Delta (V a b _)) -> (a, b), \(Delta (V a b _)) -> (a, b))

hom2 :: V -> Lim D
hom2 (V a b _) = Lim a b
