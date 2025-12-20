module Product where

import Data.Kind

type data J = J1 | J2

type Delta :: Type -> (J -> Type)
newtype Delta v a = Delta v

type Lim :: (J -> Type) -> Type
data Lim f = Lim (f J1) (f J2)

type (~>) :: (J -> Type) -> (J -> Type) -> Type
type f ~> g = forall (j :: J). f j -> g j

type Adjunction :: (Type -> (J -> Type)) -> ((J -> Type) -> Type) -> Constraint
class Adjunction f g | f -> g, g -> f where
  leftAdjunct :: forall (v :: Type) (d :: J -> Type). (f v ~> d) -> (v -> g d)
  rightAdjunct :: forall (v :: Type) (d :: J -> Type). (v -> g d) -> (f v ~> d)

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

homDeltaVtoD :: (Delta V J1 -> D J1, Delta V J2 -> D J2)
homDeltaVtoD = (\(Delta (V a _ _)) -> a, \(Delta (V _ b _)) -> b)

homVtoLimD :: V -> Lim D
homVtoLimD (V a b _) = Lim a b

homVtoLimD' :: V -> Lim D
homVtoLimD' = leftAdjunct' homDeltaVtoD

homDeltaVtoD' :: (Delta V J1 -> D J1, Delta V J2 -> D J2)
homDeltaVtoD' = rightAdjunct' homVtoLimD
