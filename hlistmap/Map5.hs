module Map5 where

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

type Map :: [Type] -> Constraint
class Map objectTypes where
  map ::
    (forall nameType. Object nameType -> nameType) ->
    HList objectTypes ->
    HList (MapTypes objectTypes)

instance Map '[] where
  map ::
    (forall nameType. Object nameType -> nameType) ->
    HList '[] ->
    HList '[]
  map _ HNil = HNil

instance (Map objectTypes) => Map (Object nameType ': objectTypes) where
  map ::
    (forall nameType'. Object nameType' -> nameType') ->
    HList (Object nameType ': objectTypes) ->
    HList (nameType ': MapTypes objectTypes)
  map f (HCons object objects) = HCons (f object) (map f objects)

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = map name exampleObjects
