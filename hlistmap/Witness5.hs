module Witness5 where

import Data.Kind
import HList
import Literal
import Object
import Objects
import Prelude hiding (map)

type IsObject :: Type -> Type -> Type
data IsObject objectType elementType where
  IsObject :: IsObject (Object nameType) nameType
  IsObject' :: IsObject (Object' ageType titleType) titleType

type AreObjects :: [Type] -> [Type] -> Type
data AreObjects objectTypes elementTypes where
  AreObjectsNil :: AreObjects '[] '[]
  AreObjectsCons ::
    IsObject objectType elementType ->
    AreObjects objectTypes elementTypes ->
    AreObjects (objectType ': objectTypes) (elementType ': elementTypes)

map ::
  AreObjects objectTypes elementTypes ->
  ( forall objectType elementType.
    IsObject objectType elementType ->
    objectType ->
    elementType
  ) ->
  HList objectTypes ->
  HList elementTypes
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
    ( \cases
        IsObject object -> name object
        IsObject' object' -> title object'
    )
    exampleObjects

mappedTitles :: HList [Literal "a", Literal "b", Literal "c"]
mappedTitles =
  map
    ( AreObjectsCons
        IsObject'
        ( AreObjectsCons
            IsObject'
            (AreObjectsCons IsObject' AreObjectsNil)
        )
    )
    ( \cases
        IsObject object -> name object
        IsObject' object' -> title object'
    )
    exampleObjects'
