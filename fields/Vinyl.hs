{-# LANGUAGE DataKinds, OverloadedLabels #-}

module Vinyl where

import Control.Lens
import Data.Vinyl
import Data.Vinyl.Syntax ()

type Person = FieldRec '["name" ::: Name, "age" ::: Int]

type Name = FieldRec '["first" ::: String, "last" ::: String]

person :: Person
person = #name =:= (     #first =:= "Tiger"
                     <+> #last =:= "Scott"
                   )
     <+> #age =:= 10

f :: String
f = person ^. #name . #first

a :: Int
a = person ^. #age

p1, p2 :: Person
p1 = person & #name . #first .~ "Micheal"
p2 = person & #age .~ 11
