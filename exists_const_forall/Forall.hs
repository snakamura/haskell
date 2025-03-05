module Forall where

import Data.Kind

type (~>) f g = forall x. f x -> g x

type Const :: Type -> Type -> Type
newtype Const a x = MkConst a deriving (Show)

type Forall :: (k -> Type) -> Type
newtype Forall f where
  MkForall :: (forall x. f x) -> Forall f

-- We cannot use `x` when you try to get `f x` from `a` (`Const a x`) in `g`,
-- as you can see in `g1`, because we cannot get a value of `x` from nowhere.
-- This means we can get `forall x. f x` from `a`.
leftAdjunct :: forall f a. (Const a ~> f) -> (a -> Forall f)
-- leftAdjunct :: forall f a. (forall x. Const a x -> f x) -> (a -> Forall f)
leftAdjunct g = \a -> MkForall (g (MkConst a))

-- We cannot use `x` when you try to get `forall x. f x` from `a` in `h`,
-- as you can see in `h1`, because we cannot get a value of `x` from nowhere.
-- This means we can get `f x` from `a` (`Const a x`).
rightAdjunct :: forall f a. (a -> Forall f) -> (Const a ~> f)
-- rightAdjunct :: forall f a. (a -> Forall f) -> (forall x. Const a x -> f x)
rightAdjunct h = \(MkConst a) -> let MkForall fx = h a in fx

g1 :: Const Bool x -> Maybe x
g1 (MkConst _) = Nothing

h1 :: Bool -> Forall Maybe
h1 _ = MkForall Nothing

-- leftAdjunct g1 == h1
-- rightAdjunct h1 == g1

g2 :: Const Bool x -> Const Int x
g2 (MkConst True) = MkConst 1
g2 (MkConst False) = MkConst 0

h2 :: Bool -> Forall (Const Int)
h2 True = MkForall (MkConst 1)
h2 False = MkForall (MkConst 0)

-- leftAdjunct g2 == h2
-- rightAdjunct h2 == g2
