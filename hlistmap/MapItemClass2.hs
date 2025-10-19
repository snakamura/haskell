module MapItemClass2 where

import Data.Kind
import HList
import Literal
import Object
import Objects
import Prelude hiding (map)

type TyFun :: Type -> Type -> Type
data TyFun a b

type a ~> b = TyFun a b -> Type

type Apply :: (a ~> b) -> a -> b
type family Apply f x

type (@@) :: (a ~> b) -> a -> b
type f @@ x = Apply f x
infixr @@

type MapItem :: Type -> Constraint
class MapItem objectType where
  type Mapped objectType :: Type
  mapItem ::
    (forall nameType. Object nameType -> nameType) ->
    objectType ->
    Mapped objectType

type MappedSym0 :: Type ~> Type
data MappedSym0 t

type instance Apply MappedSym0 x = Mapped x

instance MapItem (Object nameType) where
  type Mapped (Object nameType) = nameType
  mapItem ::
    (forall nameType'. Object nameType' -> nameType') ->
    Object nameType ->
    nameType
  mapItem f = f

type MapTypes :: (Type ~> Type) -> [Type] -> [Type]
type family MapTypes f ts where
  MapTypes _ '[] = '[]
  MapTypes f (t ': ts) = f @@ t ': MapTypes f ts

type All :: (Type -> Constraint) -> [Type] -> Constraint
type family All c ts where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

map ::
  (All MapItem objectTypes) =>
  (forall nameType. Object nameType -> nameType) ->
  HList objectTypes ->
  HList (MapTypes MappedSym0 objectTypes)
map _ HNil = HNil
map f (HCons object objects) = HCons (mapItem f object) (map f objects)

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = map name exampleObjects
