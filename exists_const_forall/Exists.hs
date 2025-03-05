module Exists where

import Data.Kind

type (~>) f g = forall x. f x -> g x

type Exists :: (k -> Type) -> Type
data Exists f where
  MkExists :: f x -> Exists f

type Const :: Type -> Type -> Type
newtype Const a x = MkConst a deriving (Show)

-- We cannot use `x` when you try to get `a` from `Exist f` in `g`,
-- as you can see in `g1`, because it's existential.
-- This means we can get `a` from `f` whatever `x` is.
leftAdjunct :: forall f a. (Exists f -> a) -> (f ~> Const a)
-- leftAdjunct :: forall f a. (Exists f -> a) -> (forall x. f x -> Const a x)
leftAdjunct g = \fx -> MkConst (g (MkExists fx))

-- We cannot use `x` when you try to get `a` (`Const a x`) from `f x` in `h`,
-- as you can see in `h1`, because it should work with any `x`.
-- This means we can get `a` from `Exist f` whatever `x` hidden in `Exist f` is.
rightAdjunct :: forall f a. (f ~> Const a) -> (Exists f -> a)
-- rightAdjunct :: forall f a. (forall x. f x -> Const a x) -> (Exists f -> a)
rightAdjunct h = \(MkExists fa) -> let MkConst a = h fa in a

g1 :: Exists Maybe -> Bool
g1 (MkExists (Just _)) = True
g1 (MkExists Nothing) = False

h1 :: Maybe x -> Const Bool x
h1 (Just _) = MkConst True
h1 Nothing = MkConst False

-- leftAdjunct g1 == h1
-- rightAdjunct h1 == g1
