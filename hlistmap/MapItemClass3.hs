module MapItemClass3 where

import Data.Kind
import HList
import Literal
import Object
import Objects
import Prelude hiding (map)

type TyFun :: k -> l -> Type
data TyFun a b

type (~>) :: k -> l -> Type
type a ~> b = TyFun a b -> Type

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

type MapItem :: (Type -> Type) -> Type -> Constraint
class MapItem objectTypeCon objectType where
  type ResultType objectType :: Type
  mapItem ::
    (forall elementType. objectTypeCon elementType -> elementType) ->
    objectType ->
    ResultType objectType

type ResultTypeSym0 :: Type ~> Type
data ResultTypeSym0 t

type instance Apply ResultTypeSym0 x = ResultType x

map ::
  All (MapItem objectType) objectTypes =>
  (forall nameType. objectType nameType -> nameType) ->
  HList objectTypes ->
  HList (MapTypes ResultTypeSym0 objectTypes)
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
