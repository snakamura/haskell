module Maybe where

import Data.Kind

type Maybe :: Type -> Type
data Maybe a = Nothing | Just a deriving (Functor)
