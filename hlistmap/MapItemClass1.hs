module MapItemClass1 where

import Data.Kind
import HList
import Literal
import Object
import Objects
import Prelude hiding (map)

type MapItem :: Type -> Constraint
class MapItem objectType where
  type ResultType objectType :: Type
  mapItem ::
    (forall nameType. Object nameType -> nameType) ->
    objectType ->
    ResultType objectType

instance MapItem (Object nameType) where
  type ResultType (Object nameType) = nameType
  mapItem ::
    (forall nameType'. Object nameType' -> nameType') ->
    Object nameType ->
    nameType
  mapItem f = f

type ResultTypes :: [Type] -> [Type]
type family ResultTypes ts where
  ResultTypes '[] = '[]
  ResultTypes (t ': ts) = ResultType t ': ResultTypes ts

type All :: (Type -> Constraint) -> [Type] -> Constraint
type family All c ts where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

map ::
  (All MapItem objectTypes) =>
  (forall nameType. Object nameType -> nameType) ->
  HList objectTypes ->
  HList (ResultTypes objectTypes)
map _ HNil = HNil
map f (HCons object objects) = HCons (mapItem f object) (map f objects)

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = map name exampleObjects
