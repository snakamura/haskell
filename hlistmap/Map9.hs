module Map9 where

import Data.Kind
import GHC.Base hiding (map)
import GHC.TypeLits
import Data.Proxy
import Prelude hiding (map)

type HList :: [Type] -> Type
data HList xs where
    HNil :: HList '[]
    HCons :: x -> HList xs -> HList (x ': xs)

infixr `HCons`

type Literal :: Symbol -> Type
newtype Literal n = Literal String

makeLiteral :: forall (n :: Symbol) -> KnownSymbol n => Literal n
makeLiteral n = Literal (symbolVal (Proxy @n))

makeLiteral' :: KnownSymbol n => Literal n
makeLiteral' @n = Literal (symbolVal (Proxy @n))

type Object :: Type -> Type
newtype Object n = Object { name :: n }

objects :: HList [Object (Literal "a"), Object (Literal "b"), Object (Literal "c")]
objects = Object { name = makeLiteral "a" } `HCons`
          Object { name = makeLiteral "b" } `HCons`
          Object { name = makeLiteral "c" } `HCons`
          HNil

type TyFun :: Type -> Type -> Type
data TyFun a b

type a ~> b = TyFun a b -> Type

type Apply :: (a ~> b) -> a -> b
type family Apply f x

type (@@) :: (a ~> b) -> a -> b
type f @@ x = Apply f x
infixr @@

type MapItem :: Type -> Constraint
class MapItem o where
    type Mapped o :: Type
    mapItem :: (forall n. Object n -> n) -> o -> Mapped o

type MappedSym0 :: Type ~> Type
data MappedSym0 t

type instance Apply MappedSym0 x = Mapped x

instance MapItem (Object n) where
    type Mapped (Object n) = n
    mapItem :: (forall n'. Object n' -> n') -> Object n -> n
    mapItem f = f

type MapAll :: (Type ~> Type) -> [Type] -> [Type]
type family MapAll f xs where
    MapAll _ '[] = '[]
    MapAll f (x ': xs) = f @@ x ': MapAll f xs

type All :: (Type -> Constraint) -> [Type] -> Constraint
type family All c xs where
    All c '[] = ()
    All c (x ': xs) = (c x, All c xs)

map :: All MapItem xs => (forall n. Object n -> n) -> HList xs -> HList (MapAll MappedSym0 xs)
map _ HNil = HNil
map f (HCons x xs) = HCons (mapItem f x) (map f xs)

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames = map name objects
