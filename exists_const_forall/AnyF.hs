module AnyF where

import Data.Kind

type AnyF :: (k -> Type) -> Type
newtype AnyF f = MkAnyF (forall a. f a)

anyFValue :: AnyF []
anyFValue = MkAnyF []

fromAnyF :: AnyF [] -> Int
fromAnyF (MkAnyF l) = let intList :: [Int] = l in length intList

toAnyF :: Int -> AnyF []
toAnyF _ = MkAnyF []

forward :: (forall x. a -> f x) -> (a -> AnyF f)
forward g = \a -> MkAnyF (g a)

backward :: (a -> AnyF f) -> (forall x. a -> f x)
backward h = \a -> let MkAnyF fx = h a in fx

newtype Const a x = MkConst a

forward' :: (forall x. Const a x -> f x) -> (a -> AnyF f)
forward' g = \a -> MkAnyF (g (MkConst a))

backward' :: (a -> AnyF f) -> (forall x. Const a x -> f x)
backward' h = \(MkConst a) -> let MkAnyF fx = h a in fx

type f ~> g = forall x. f x -> g x

leftAdjunct :: (Const a ~> f) -> (a -> AnyF f)
leftAdjunct g = \a -> MkAnyF (g (MkConst a))

rightAdjunct :: (a -> AnyF f) -> (Const a ~> f)
rightAdjunct h = \(MkConst a) -> let MkAnyF fx = h a in fx

fmap' :: Functor f => f a -> (a -> b) -> f b
fmap' = flip fmap

fmap'' :: Functor f => f a -> (a -> x) -> f x
fmap'' = fmap'

type AnyFA :: (Type -> Type) -> Type -> Type
newtype AnyFA f a = MkAnyFA (forall x. (a -> x) -> f x)

fmap''' :: Functor f => f a -> AnyFA f a
fmap''' fa = MkAnyFA (fmap'' fa)

fmap'''' :: Functor f => f ~> AnyFA f
fmap'''' fa = MkAnyFA (fmap'' fa)

