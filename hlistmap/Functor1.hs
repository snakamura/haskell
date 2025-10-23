module Functor1 where

import Data.Functor.Identity
import Data.Kind

import Literal
import Object

type HList :: (Type -> Type) -> [Type] -> Type
data HList f xs where
    HNil :: HList f '[]
    HCons :: f x -> HList f xs -> HList f (x ': xs)

infixr `HCons`

exampleObjects :: HList Object [Literal "a", Literal "b", Literal "c"]
exampleObjects =
  Object {name = Literal @"a"}
    `HCons` Object {name = Literal @"b"}
    `HCons` Object {name = Literal @"c"}
    `HCons` HNil

mapName :: HList Object xs -> HList Identity xs
mapName HNil = HNil
mapName (HCons x xs) = HCons (Identity (name x)) (mapName xs)

mappedNames :: HList Identity [Literal "a", Literal "b", Literal "c"]
mappedNames = mapName exampleObjects
