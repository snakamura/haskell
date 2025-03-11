module AnyC where

import Data.Kind

type AnyC :: (Type -> Constraint) -> Type
newtype AnyC c = MkAnyC (forall a. (c a) => a)

anyCNumValue :: AnyC Num
anyCNumValue = MkAnyC 1

fromAnyCNum :: AnyC Num -> Int
fromAnyCNum (MkAnyC a) = a

toAnyCNum :: Int -> AnyC Num
toAnyCNum n = MkAnyC (fromInteger (toInteger n))

anyCReadValue :: AnyC Read
anyCReadValue = MkAnyC (read "1")

fromAnyCShow :: AnyC Show -> String
fromAnyCShow (MkAnyC a) = let n = a :: Int in show n

toAnyCRead :: String -> AnyC Read
toAnyCRead s = MkAnyC (read s)

forward :: (forall x. c x => a -> x) -> (a -> AnyC c)
forward g = \a -> MkAnyC (g a)

backward :: (a -> AnyC c) -> (forall x. c x => a -> x)
backward h = \a -> let MkAnyC x = h a in x
