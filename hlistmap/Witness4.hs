module Witness4 where

import Data.Kind
import HList
import Literal
import Object
import Objects
import Prelude hiding (map)

type IsObject :: Type -> Type -> Type
data IsObject objectType nameType where
  IsObject :: IsObject (Object nameType) nameType

type AreObjects :: [Type] -> [Type] -> Type
data AreObjects objectTypes nameTypes where
  AreObjectsNil :: AreObjects '[] '[]
  AreObjectsCons ::
    IsObject objectType nameType ->
    AreObjects objectTypes nameTypes ->
    AreObjects (objectType ': objectTypes) (nameType ': nameTypes)

type IsObjectC :: Type -> Type -> Constraint
class IsObjectC objectType nameType where
  isObject :: objectType -> IsObject objectType nameType

instance IsObjectC (Object nameType) nameType where
  isObject :: Object nameType -> IsObject (Object nameType) nameType
  isObject _ = IsObject

type AreObjectsC :: [Type] -> [Type] -> Constraint
class AreObjectsC objectTypes nameTypes where
  areObjects :: HList objectTypes -> AreObjects objectTypes nameTypes

instance AreObjectsC '[] '[] where
  areObjects :: HList '[] -> AreObjects '[] '[]
  areObjects HNil = AreObjectsNil

instance
  (IsObjectC objectType nameType, AreObjectsC objectTypes nameTypes) =>
  AreObjectsC (objectType ': objectTypes) (nameType ': nameTypes)
  where
  areObjects ::
    HList (objectType ': objectTypes) ->
    AreObjects (objectType ': objectTypes) (nameType ': nameTypes)
  areObjects (HCons object objects) =
    AreObjectsCons (isObject object) (areObjects objects)

map ::
  AreObjects objectTypes nameTypes ->
  ( forall objectType nameType.
    IsObject objectType nameType ->
    objectType ->
    nameType
  ) ->
  HList objectTypes ->
  HList nameTypes
map AreObjectsNil _ HNil = HNil
map (AreObjectsCons isObject' areObjects') f (HCons object objects) =
  HCons
    (f isObject' object)
    (map areObjects' f objects)

mapC ::
  (AreObjectsC objectTypes nameTypes) =>
  ( forall objectType nameType.
    IsObject objectType nameType ->
    objectType ->
    nameType
  ) ->
  HList objectTypes ->
  HList nameTypes
mapC f objects = map (areObjects objects) f objects

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = mapC (\IsObject object -> name object) exampleObjects
