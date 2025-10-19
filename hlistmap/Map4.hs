module Map4 where

import Data.Kind
import HList
import Literal
import Object
import Objects
import Prelude hiding (map)

type MapTypes :: (Type -> Type) -> [Type] -> [Type]
type family MapTypes objectTypeCon objectTypes

type instance MapTypes _ '[] = '[]

type instance
  MapTypes Object (Object nameType ': objectTypes) =
    nameType ': MapTypes Object objectTypes

type Map :: (Type -> Type) -> [Type] -> Constraint
class Map objectTypeCon objectTypes where
  map ::
    (forall nameType. objectTypeCon nameType -> nameType) ->
    HList objectTypes ->
    HList (MapTypes objectTypeCon objectTypes)

instance Map objectTypeCon '[] where
  map ::
    (forall nameType. objectTypeCon nameType -> nameType) ->
    HList '[] ->
    HList '[]
  map _ HNil = HNil

instance
  (Map Object objectTypes) =>
  Map Object (Object nameType ': objectTypes)
  where
  map ::
    (forall nameType'. Object nameType' -> nameType') ->
    HList (Object nameType ': objectTypes) ->
    HList (nameType ': MapTypes Object objectTypes)
  map f (HCons object objects) = HCons (f object) (map f objects)

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = map name exampleObjects

type instance
  MapTypes (Object' ageType) (Object' ageType titleType ': objectTypes) =
    titleType ': MapTypes (Object' ageType) objectTypes

instance
  (Map (Object' ageType) objectTypes) =>
  Map (Object' ageType) (Object' ageType titleType ': objectTypes)
  where
  map ::
    (forall titleType'. Object' ageType titleType' -> titleType') ->
    HList (Object' ageType titleType ': objectTypes) ->
    HList (titleType ': MapTypes (Object' ageType) objectTypes)
  map f (HCons object objects) = HCons (f object) (map f objects)

mappedTitles :: HList [Literal "a", Literal "b", Literal "c"]
mappedTitles = map (title @Int) exampleObjects'
