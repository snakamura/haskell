module Map8 where

import Data.Kind
import HList
import Literal
import Object
import Objects
import Prelude hiding (map)

type MapItem :: Type -> Constraint
class MapItem objectType where
  type Mapped objectType :: Type
  mapItem ::
    (forall nameType. Object nameType -> nameType) ->
    objectType ->
    Mapped objectType

instance MapItem (Object nameType) where
  type Mapped (Object nameType) = nameType
  mapItem ::
    (forall nameType'. Object nameType' -> nameType') ->
    Object nameType ->
    nameType
  mapItem f = f

type MapTypes :: [Type] -> [Type]
type family MapTypes ts where
  MapTypes '[] = '[]
  MapTypes (t ': ts) = Mapped t ': MapTypes ts

type All :: (Type -> Constraint) -> [Type] -> Constraint
type family All c ts where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

map ::
  (All MapItem objectTypes) =>
  (forall nameType. Object nameType -> nameType) ->
  HList objectTypes ->
  HList (MapTypes objectTypes)
map _ HNil = HNil
map f (HCons object objects) = HCons (mapItem f object) (map f objects)

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = map name exampleObjects
