module MapClass3 where

import Data.Kind
import HList
import Literal
import Object
import Objects
import Prelude hiding (map)

type Map :: (Type -> Type) -> [Type] -> Constraint
class Map objectTypeCon objectTypes where
  type ResultType objectTypes :: [Type]
  map ::
    (forall elementType. objectTypeCon elementType -> elementType) ->
    HList objectTypes ->
    HList (ResultType objectTypes)

instance Map objectTypeCon '[] where
  type ResultType '[] = '[]
  map ::
    (forall elementType. objectTypeCon elementType -> elementType) ->
    HList '[] ->
    HList '[]
  map _ HNil = HNil

instance
  (Map Object objectTypes) =>
  Map Object (Object nameType ': objectTypes)
  where
  type
    ResultType (Object nameType ': objectTypes) =
      nameType ': ResultType objectTypes
  map ::
    (forall nameType'. Object nameType' -> nameType') ->
    HList (Object nameType ': objectTypes) ->
    HList (nameType ': ResultType objectTypes)
  map f (HCons object objects) = HCons (f object) (map f objects)

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = map name exampleObjects

instance
  (Map (Object' ageType) objectTypes) =>
  Map (Object' ageType) (Object' ageType titleType ': objectTypes)
  where
  type ResultType (Object' ageType titleType ': objectTypes) =
    titleType ': ResultType objectTypes
  map ::
    (forall titleType'. Object' ageType titleType' -> titleType') ->
    HList (Object' ageType titleType ': objectTypes) ->
    HList (titleType ': ResultType objectTypes)
  map f (HCons object objects) = HCons (f object) (map f objects)

mappedTitles :: HList [Literal "a", Literal "b", Literal "c"]
mappedTitles = map (title @Int) exampleObjects'
