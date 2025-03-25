module Any where

import Data.Kind

type Any :: Type
newtype Any = MkAny (forall a. a)

anyValue :: Any
anyValue = MkAny undefined

fromAny :: Any -> Int
fromAny (MkAny a) = a

toAny :: Int -> Any
toAny _ = MkAny undefined

forward :: (forall x. a -> x) -> (a -> Any)
forward g = \a -> MkAny (g a)

backward :: (a -> Any) -> (forall x. a -> x)
backward h = \a -> let MkAny x = h a in x
