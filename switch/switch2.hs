{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Kind (Type)

data A = A
data B = B

type SBool :: Bool -> Type
data SBool b where
  STrue :: SBool 'True
  SFalse :: SBool 'False

data SomeBool where
  SomeBool :: SBool b -> SomeBool

type R :: Bool -> Type
type family R b where
  R 'True = A
  R 'False = B

value :: SBool b -> R b
value STrue = A
value SFalse = B

func :: SBool b -> R b -> String
func STrue A = "A"
func SFalse B = "B"

test :: Bool -> String
test useA =
  let sc = if useA then SomeBool STrue else SomeBool SFalse
   in case sc of
        SomeBool sb -> func sb (value sb)
