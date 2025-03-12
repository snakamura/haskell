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
