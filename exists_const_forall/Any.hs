module Any where

import Data.Kind

type Any :: Type
newtype Any = MkAny (forall a. a)

anyInt :: Any -> Int
anyInt (MkAny a) = a

anyValue :: Any
anyValue = MkAny undefined
