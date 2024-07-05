{-# LANGUAGE TemplateHaskell #-}

module Lens where

import Control.Lens

data Person = Person
  { _name :: Name,
    _age :: Int
  }
  deriving (Show)

data Name = Name
  { _first :: String,
    _last :: String
  }
  deriving (Show)

makeLenses ''Person
makeLenses ''Name

person :: Person
person = Person (Name "Tiger" "Scott") 10

f :: String
f = person ^. name . first

a :: Int
a = person ^. age

p1, p2 :: Person
p1 = person & name . first .~ "Micheal"
p2 = person & age .~ 11
