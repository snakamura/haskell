module Witness3 where

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
map (AreObjectsCons isObject areObjects) f (HCons object objects) =
  HCons
    (f isObject object)
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
    (\IsObject object -> name object)
    exampleObjects
