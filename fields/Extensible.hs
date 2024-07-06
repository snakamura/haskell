{-# LANGUAGE DataKinds, OverloadedLabels #-}

module Extensible where

import Control.Lens
import Data.Extensible

type Person = Record '[ "name" >: Name
                      , "age" >: Int
                      ]

type Name = Record '[ "first" >: String
                    , "last" >: String
                    ]

person :: Person
person = #name @= (    #first @= "Tiger"
                    <: #last @= "Scott"
                    <: nil
                  )
      <: #age @= 10
      <: nil

f :: String
f = person ^. xlb #name . xlb #first

a :: Int
a = person ^. #age

p1, p2 :: Person
p1 = person & xlb #name . xlb #first .~ "Micheal"
p2 = person & #age .~ 11
