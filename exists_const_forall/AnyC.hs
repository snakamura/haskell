module AnyC where

import Data.Kind

type AnyC :: (Type -> Constraint) -> Type
newtype AnyC c = MkAnyC (forall a. (c a) => a)

anyCNum :: AnyC Num -> Int
anyCNum (MkAnyC a) = a

anyCNumValue :: AnyC Num
anyCNumValue = MkAnyC 1

anyCRead :: AnyC Read -> String
anyCRead (MkAnyC a) = let n = a :: Int in show n

anyCReadValue :: AnyC Read
anyCReadValue = MkAnyC (read "1")
