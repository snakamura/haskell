{-# LANGUAGE DataKinds, OverloadedLabels #-}

module Field where

import GHC.Records
import GHC.OverloadedLabels

data Person = Person
  { name :: Name,
    age :: Int
  }
  deriving (Show)

data Name = Name
  { first :: String,
    last :: String
  }
  deriving (Show)

instance HasField x r a => IsLabel x (r -> a) where
  fromLabel = getField @x

person :: Person
person = Person (Name "Tiger" "Scott") 10

f1, f2 :: String
f1 = getField @"first" $ getField @"name" person
f2 = #first $ #name person

a :: Int
a = getField @"age" person
