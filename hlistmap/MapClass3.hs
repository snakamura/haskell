module MapClass3 where

import Data.Kind
import HList
import Literal
import Object
import Objects
import Prelude hiding (map)

type MapTypes :: [Type] -> [Type]
type family MapTypes objectTypes

type instance MapTypes '[] = '[]

type instance
  MapTypes (Object nameType ': objectTypes) =
    nameType ': MapTypes objectTypes

type Map :: (Type -> Type) -> [Type] -> Constraint
class Map objectTypeCon objectTypes where
  map ::
    (forall nameType. objectTypeCon nameType -> nameType) ->
    HList objectTypes ->
    HList (MapTypes objectTypes)

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
    HList (nameType ': MapTypes objectTypes)
  map f (HCons object objects) = HCons (f object) (map f objects)

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = map name exampleObjects

type instance
  MapTypes (Object' ageType titleType ': objectTypes) =
    titleType ': MapTypes objectTypes

instance
  (Map (Object' ageType) objectTypes) =>
  Map (Object' ageType) (Object' ageType titleType ': objectTypes)
  where
  map ::
    (forall titleType'. Object' ageType titleType' -> titleType') ->
    HList (Object' ageType titleType ': objectTypes) ->
    HList (titleType ': MapTypes objectTypes)
  map f (HCons object objects) = HCons (f object) (map f objects)

mappedTitles :: HList [Literal "a", Literal "b", Literal "c"]
mappedTitles = map (title @Int) exampleObjects'
