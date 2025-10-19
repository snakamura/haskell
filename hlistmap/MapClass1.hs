module MapClass1 where

import Data.Kind
import HList
import Literal
import Object
import Objects

type MapNameTypes :: [Type] -> [Type]
type family MapNameTypes objectTypes where
  MapNameTypes '[] = '[]
  MapNameTypes (Object nameType ': objectTypes) =
    nameType ': MapNameTypes objectTypes

type MapNames :: [Type] -> Constraint
class MapNames objectTypes where
  mapNames :: HList objectTypes -> HList (MapNameTypes objectTypes)

instance MapNames '[] where
  mapNames :: HList '[] -> HList '[]
  mapNames HNil = HNil

instance
  (MapNames objectTypes) =>
  MapNames (Object nameType ': objectTypes)
  where
  mapNames ::
    HList (Object nameType ': objectTypes) ->
    HList (nameType ': MapNameTypes objectTypes)
  mapNames (HCons object objects) = HCons (name object) (mapNames objects)

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = mapNames exampleObjects
