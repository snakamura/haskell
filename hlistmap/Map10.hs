module Map10 where

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

type MapItem :: (Type -> Type) -> Type -> Constraint
class MapItem f o where
  type Mapped o :: Type
  mapItem :: (forall n. f n -> n) -> o -> Mapped o

type MappedSym0 :: Type ~> Type
data MappedSym0 t

type instance Apply MappedSym0 x = Mapped x

instance MapItem Object (Object nameType) where
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
  All (MapItem Object) objectTypes =>
  (forall nameType. Object nameType -> nameType) ->
  HList objectTypes ->
  HList (MapTypes MappedSym0 objectTypes)
map _ HNil = HNil
map f (HCons x xs) = HCons (mapItem f x) (map f xs)

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = map name exampleObjects
