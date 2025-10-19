module Witness3 where

import Data.Kind
import HList
import Literal
import Object
import Objects
import Prelude hiding (map)

type MapTypes :: (Type -> Type) -> [Type] -> [Type]
type family MapTypes objectTypeCon objectTypes where
  MapTypes _ '[] = '[]
  MapTypes Object (Object nameType ': objectTypes) =
    nameType ': MapTypes Object objectTypes

data IsObject objectType where
  IsObject :: IsObject (Object nameType)

data AreObjects objectTypes where
  AreObjectsNil :: AreObjects '[]
  AreObjectsCons ::
    IsObject objectType ->
    AreObjects objectTypes ->
    AreObjects (objectType ': objectTypes)

class IsObjectC objectType where
  isObject :: objectType -> IsObject objectType

instance IsObjectC (Object nameType) where
  isObject :: Object nameType -> IsObject (Object nameType)
  isObject _ = IsObject

class AreObjectsC objectTypes where
  areObjects :: HList objectTypes -> AreObjects objectTypes

instance AreObjectsC '[] where
  areObjects :: HList '[] -> AreObjects '[]
  areObjects HNil = AreObjectsNil

instance
  (IsObjectC objectType, AreObjectsC objectTypes) =>
  AreObjectsC (objectType ': objectTypes)
  where
  areObjects ::
    HList (objectType ': objectTypes) ->
    AreObjects (objectType ': objectTypes)
  areObjects (HCons object objects) =
    AreObjectsCons (isObject object) (areObjects objects)

map ::
  AreObjects objectTypes ->
  (forall nameType. Object nameType -> nameType) ->
  HList objectTypes ->
  HList (MapTypes Object objectTypes)
map AreObjectsNil _ HNil = HNil
map (AreObjectsCons IsObject o) f (HCons object objects) =
  HCons (f object) (map o f objects)

mapC ::
  (AreObjectsC objectTypes) =>
  (forall nameType. Object nameType -> nameType) ->
  HList objectTypes ->
  HList (MapTypes Object objectTypes)
mapC f objects = map (areObjects objects) f objects

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = mapC name exampleObjects
