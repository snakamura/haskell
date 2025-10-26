module MapItemClass2 where

import Data.Kind
import HList
import Literal
import Object
import Objects
import Prelude hiding (map)

type a ~> b = (a -> b) -> Type

type Apply :: (a ~> b) -> a -> b
type family Apply f x

type (@@) :: (a ~> b) -> a -> b
type f @@ x = Apply f x
infixr @@

type MapTypes :: (Type ~> Type) -> [Type] -> [Type]
type family MapTypes f ts where
  MapTypes _ '[] = '[]
  MapTypes f (t ': ts) = f @@ t ': MapTypes f ts

type All :: (Type -> Constraint) -> [Type] -> Constraint
type family All c ts where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

type MapItem :: Type -> Constraint
class MapItem objectType where
  type ResultType objectType :: Type
  mapItem ::
    (forall nameType. Object nameType -> nameType) ->
    objectType ->
    ResultType objectType

type ResultTypeSym0 :: Type ~> Type
data ResultTypeSym0 t

type instance Apply ResultTypeSym0 x = ResultType x

map ::
  (All MapItem objectTypes) =>
  (forall nameType. Object nameType -> nameType) ->
  HList objectTypes ->
  HList (MapTypes ResultTypeSym0 objectTypes)
map _ HNil = HNil
map f (HCons object objects) = HCons (mapItem f object) (map f objects)

instance MapItem (Object nameType) where
  type ResultType (Object nameType) = nameType
  mapItem ::
    (forall nameType'. Object nameType' -> nameType') ->
    Object nameType ->
    nameType
  mapItem f = f

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = map name exampleObjects
