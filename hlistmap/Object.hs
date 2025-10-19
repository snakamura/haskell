module Object
  ( Object (..),
    Object' (..),
  )
where

import Data.Kind

type Object :: Type -> Type
newtype Object nameType = Object
  { name :: nameType
  }

type Object' :: Type -> Type -> Type
data Object' ageType titleType = Object'
  { age :: ageType,
    title :: titleType
  }
