{-# LANGUAGE DataKinds, OverloadedLabels #-}

module GeneircLens where

import Control.Lens
import Data.Generics.Labels ()
import Data.Generics.Product
import GHC.Generics

data Person = Person
  { name :: Name,
    age :: Int
  }
  deriving (Show, Generic)

data Name = Name
  { first :: String,
    last :: String
  }
  deriving (Show, Generic)

person :: Person
person = Person (Name "Tiger" "Scott") 10

f1, f2 :: String
f1 = person ^. field @"name" . field @"first"
f2 = person ^. #name . #first

a1, a2 :: Int
a1 = person ^. field @"age"
a2 = person ^. #age

p1, p2, p3 :: Person
p1 = person & field @"name" . field @"first" .~ "Micheal"
p2 = person & field @"age" .~ 11
p3 = person & #name . #first .~ "Micheal"
