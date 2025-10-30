module MapItemClass4 where

import Data.Kind
import Data.List.Singletons hiding (All)
import Data.Singletons.TH
import Generics.SOP
import HList
import Literal
import Object
import Objects
import Prelude hiding (map)

type MapItem :: (Type -> Type) -> Type -> Constraint
class MapItem objectTypeCon objectType where
  type ResultType objectType :: Type
  mapItem ::
    (forall elementType. objectTypeCon elementType -> elementType) ->
    objectType ->
    ResultType objectType

genDefunSymbols [''ResultType]

map ::
  (All (MapItem objectType) objectTypes) =>
  (forall nameType. objectType nameType -> nameType) ->
  HList objectTypes ->
  HList (Map ResultTypeSym0 objectTypes)
map _ HNil = HNil
map f (HCons x xs) = HCons (mapItem f x) (map f xs)

instance MapItem Object (Object nameType) where
  type ResultType (Object nameType) = nameType
  mapItem ::
    (forall nameType'. Object nameType' -> nameType') ->
    Object nameType ->
    nameType
  mapItem f = f

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = map name exampleObjects

instance MapItem (Object' ageType) (Object' ageType titleType) where
  type ResultType (Object' ageType titleType) = titleType
  mapItem ::
    (forall titleType'. Object' ageType titleType' -> titleType') ->
    Object' ageType titleType ->
    titleType
  mapItem f = f

mappedTitles :: HList [Literal "x", Literal "y", Literal "z"]
mappedTitles = map (title @Int) exampleObjects'
