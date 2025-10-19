module Object (Object (..)) where

import Data.Kind

type Object :: Type -> Type
newtype Object nameType = Object
  { name :: nameType
  }
