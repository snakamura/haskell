module Map6 where

import Data.Kind
import HList
import Literal
import Object
import Objects
import Prelude hiding (map)

type MapTypes :: [Type] -> [Type]
type family MapTypes objectTypes where
  MapTypes '[] = '[]
  MapTypes (Object nameType ': objectTypes) =
    nameType ': MapTypes objectTypes

data IsObject objectType where
  IsObject :: IsObject (Object nameType)

data AreObjects objectTypes where
  AreObjectsNil :: AreObjects '[]
  AreObjectsCons ::
    IsObject objectType ->
    AreObjects objectTypes ->
    AreObjects (objectType ': objectTypes)

map ::
  AreObjects objectTypes ->
  (forall nameType. Object nameType -> nameType) ->
  HList objectTypes ->
  HList (MapTypes objectTypes)
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
