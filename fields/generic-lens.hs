{-# LANGUAGE DataKinds #-}

import Control.Lens
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

f :: String
f = person ^. field @"name" . field @"first"

a :: Int
a = person ^. field @"age"

p1, p2 :: Person
p1 = person & field @"name" . field @"first" .~ "Micheal"
p2 = person & field @"age" .~ 11
