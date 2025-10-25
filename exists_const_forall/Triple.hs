module Triple where

import Data.Kind (Constraint, Type)

type FunctorType :: Type
type FunctorType = Type -> Type

type (~>) :: FunctorType -> FunctorType -> Type
type f ~> g = forall a. f a -> g a

type FunctorFromHaskToHask2Type :: Type
type FunctorFromHaskToHask2Type = Type -> FunctorType

type FunctorFromHaskToHask2 :: FunctorFromHaskToHask2Type -> Constraint
class FunctorFromHaskToHask2 t where
  mapFromHaskToHask2 ::
    (Functor (t a), Functor (t b)) =>
    (a -> b) -> (t a ~> t b)

type FunctorFromHask2ToHaskType :: Type
type FunctorFromHask2ToHaskType = FunctorType -> Type

type FunctorFromHask2ToHask :: FunctorFromHask2ToHaskType -> Constraint
class FunctorFromHask2ToHask t where
  mapFromHask2ToHask ::
    (Functor f, Functor g) =>
    (f ~> g) -> (t f -> t g)

type Const :: Type -> FunctorType
newtype Const a b = MkConst a

instance Functor (Const a) where
  fmap :: (b -> c) -> Const a b -> Const a c
  fmap _ (MkConst a) = MkConst a

instance FunctorFromHaskToHask2 Const where
  mapFromHaskToHask2 :: (a -> b) -> (Const a ~> Const b)
  mapFromHaskToHask2 a2b = \(MkConst a) -> MkConst (a2b a)

type SomeF :: FunctorType -> Type
data SomeF f = forall a. MkSomeF (f a)

instance FunctorFromHask2ToHask SomeF where
  mapFromHask2ToHask :: (f ~> g) -> (SomeF f -> SomeF g)
  mapFromHask2ToHask f2g = \(MkSomeF fa) -> MkSomeF (f2g fa)

type AnyF :: FunctorType -> Type
newtype AnyF f = MkAnyF (forall a. f a)

instance FunctorFromHask2ToHask AnyF where
  mapFromHask2ToHask :: (f ~> g) -> (AnyF f -> AnyF g)
  mapFromHask2ToHask f2g = \(MkAnyF fa) -> MkAnyF (f2g fa)

class
  (FunctorFromHask2ToHask f, FunctorFromHaskToHask2 g) =>
  LeftAdjunction f g
    | f -> g,
      g -> f
  where
  leftLeftAdjunct :: (Functor h) => (f h -> a) -> (h ~> g a)
  rightLeftAdjunct :: (Functor h) => (h ~> g a) -> (f h -> a)

class
  (FunctorFromHaskToHask2 f, FunctorFromHask2ToHask g) =>
  RightAdjunction f g
    | f -> g,
      g -> f
  where
  leftRightAdjunct :: (Functor h) => (f a ~> h) -> (a -> g h)
  rightRightAdjunct :: (Functor h) => (a -> g h) -> (f a ~> h)

instance LeftAdjunction SomeF Const where
  leftLeftAdjunct :: (Functor h) => (SomeF h -> a) -> (h ~> Const a)
  leftLeftAdjunct someH2a = \h -> MkConst (someH2a (MkSomeF h))

  rightLeftAdjunct :: (Functor h) => (h ~> Const a) -> (SomeF h -> a)
  rightLeftAdjunct h2const = \(MkSomeF h) -> let MkConst a = h2const h in a

instance RightAdjunction Const AnyF where
  leftRightAdjunct :: (Functor h) => (Const a ~> h) -> (a -> AnyF h)
  leftRightAdjunct const2h = \a -> MkAnyF (const2h (MkConst a))

  rightRightAdjunct :: (Functor h) => (a -> AnyF h) -> (Const a ~> h)
  rightRightAdjunct a2anyH = \(MkConst a) -> let MkAnyF ha = a2anyH a in ha
