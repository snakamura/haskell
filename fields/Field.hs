{-# LANGUAGE DataKinds #-}

module Field where

import GHC.Records

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

person :: Person
person = Person (Name "Tiger" "Scott") 10

f :: String
f = getField @"first" $ getField @"name" person

a :: Int
a = getField @"age" person
