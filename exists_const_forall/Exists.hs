module Exists where

import Data.Kind

type Exists :: (k -> Type) -> Type
data Exists f where
  MkExists :: f x -> Exists f

type Const :: Type -> Type -> Type
newtype Const a x = MkConst a

forward :: forall f a. (Exists f -> a) -> (forall x. f x -> Const a x)
forward g = \fx -> MkConst (g (MkExists fx))

backward :: forall f a. (forall x. f x -> Const a x) -> (Exists f -> a)
backward h = \(MkExists fa) -> let MkConst a = h fa in a
