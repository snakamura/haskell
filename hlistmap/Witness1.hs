module Witness1 where

import Data.Kind
import HList
import Literal
import Object
import Objects
import Prelude hiding (map)

type IsObject :: Type -> Type
data IsObject objectType where
  IsObject :: IsObject (Object nameType)

type AreObjects :: [Type] -> Type
data AreObjects objectTypes where
  AreObjectsNil :: AreObjects '[]
  AreObjectsCons ::
    IsObject objectType ->
    AreObjects objectTypes ->
    AreObjects (objectType ': objectTypes)

type ResultTypes :: [Type] -> [Type]
type family ResultTypes objectTypes where
  ResultTypes '[] = '[]
  ResultTypes (Object nameType ': objectTypes) =
    nameType ': ResultTypes objectTypes

map ::
  AreObjects objectTypes ->
  (forall nameType. Object nameType -> nameType) ->
  HList objectTypes ->
  HList (ResultTypes objectTypes)
map AreObjectsNil _ HNil = HNil
map (AreObjectsCons IsObject areObjects) f (HCons object objects) =
  HCons
    (f object)
    (map areObjects f objects)

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames =
  map
    ( AreObjectsCons
        IsObject
        ( AreObjectsCons
            IsObject
            (AreObjectsCons IsObject AreObjectsNil)
        )
    )
    name
    exampleObjects
