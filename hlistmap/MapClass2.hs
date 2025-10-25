module MapClass2 where

import Data.Kind
import HList
import Literal
import Object
import Objects
import Prelude hiding (map)

type Map :: [Type] -> Constraint
class Map objectTypes where
  type ResultTypes objectTypes :: [Type]
  map ::
    (forall nameType. Object nameType -> nameType) ->
    HList objectTypes ->
    HList (ResultTypes objectTypes)

instance Map '[] where
  type ResultTypes '[] = '[]
  map ::
    (forall nameType. Object nameType -> nameType) ->
    HList '[] ->
    HList '[]
  map _ HNil = HNil

instance
  (Map objectTypes) =>
  Map (Object nameType ': objectTypes)
  where
  type
    ResultTypes (Object nameType ': objectTypes) =
      nameType ': ResultTypes objectTypes
  map ::
    (forall nameType'. Object nameType' -> nameType') ->
    HList (Object nameType ': objectTypes) ->
    HList (nameType ': ResultTypes objectTypes)
  map f (HCons object objects) = HCons (f object) (map f objects)

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = map name exampleObjects
