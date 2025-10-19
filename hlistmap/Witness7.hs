module Witness7 where

import Data.Kind
import HList
import Literal
import Object
import Objects
import Prelude hiding (map)

type IsObject :: Type -> Type -> Type
data IsObject objectType nameType where
  IsObject :: IsObject (Object nameType) nameType
  IsObject' :: IsObject (Object' ageType titleType) titleType

type AreObjects :: [Type] -> [Type] -> Type
data AreObjects objectTypes nameTypes where
  AreObjectsNil :: AreObjects '[] '[]
  AreObjectsCons ::
    IsObject objectType nameType ->
    AreObjects objectTypes nameTypes ->
    AreObjects (objectType ': objectTypes) (nameType ': nameTypes)

type IsObjectC :: Type -> Type -> Constraint
class IsObjectC objectType nameType where
  isObject :: objectType -> IsObject objectType nameType

instance IsObjectC (Object nameType) nameType where
  isObject :: Object nameType -> IsObject (Object nameType) nameType
  isObject _ = IsObject

instance IsObjectC (Object' ageType titleType) titleType where
  isObject :: Object' ageType titleType -> IsObject (Object' ageType titleType) titleType
  isObject _ = IsObject'

type AreObjectsC :: [Type] -> [Type] -> Constraint
class AreObjectsC objectTypes nameTypes where
  areObjects :: HList objectTypes -> AreObjects objectTypes nameTypes

instance AreObjectsC '[] '[] where
  areObjects :: HList '[] -> AreObjects '[] '[]
  areObjects HNil = AreObjectsNil

instance
  (IsObjectC objectType nameType, AreObjectsC objectTypes nameTypes) =>
  AreObjectsC (objectType ': objectTypes) (nameType ': nameTypes)
  where
  areObjects ::
    HList (objectType ': objectTypes) ->
    AreObjects (objectType ': objectTypes) (nameType ': nameTypes)
  areObjects (HCons object objects) =
    AreObjectsCons (isObject object) (areObjects objects)

type Arrows :: [Type] -> [Type] -> [Type]
type family Arrows xs rs where
  Arrows '[] '[] = '[]
  Arrows (x ': xs) (r ': rs) = (x -> r) ': Arrows xs rs

map ::
  AreObjects objectTypes nameTypes ->
  HList (Arrows objectTypes nameTypes) ->
  HList objectTypes ->
  HList nameTypes
map AreObjectsNil _ HNil = HNil
map (AreObjectsCons _ areObjects') (HCons f fs) (HCons object objects) =
  HCons
    (f object)
    (map areObjects' fs objects)

class BuildObjectArrows objectTypes nameTypes where
  buildObjectArrows ::
    AreObjects objectTypes nameTypes ->
    (forall nameType. Object nameType -> nameType) ->
    HList (Arrows objectTypes nameTypes)

instance BuildObjectArrows '[] '[] where
  buildObjectArrows AreObjectsNil _ = HNil

instance
  (BuildObjectArrows objectTypes nameTypes) =>
  BuildObjectArrows (Object nameType ': objectTypes) (nameType ': nameTypes)
  where
  buildObjectArrows (AreObjectsCons IsObject areObjects') f =
    HCons f (buildObjectArrows areObjects' f)

mappedNames :: HList [Literal "a", Literal "b", Literal "c"]
mappedNames =
  let areObjects' = areObjects exampleObjects
   in map
        areObjects'
        (buildObjectArrows areObjects' name)
        exampleObjects

class BuildObject'Arrows objectTypes titleTypes where
  buildObject'Arrows ::
    AreObjects objectTypes titleTypes ->
    (forall ageType titleType. Object' ageType titleType -> titleType) ->
    HList (Arrows objectTypes titleTypes)

instance BuildObject'Arrows '[] '[] where
  buildObject'Arrows AreObjectsNil _ = HNil

instance
  (BuildObject'Arrows objectTypes titleTypes) =>
  BuildObject'Arrows (Object' ageType titleType ': objectTypes) (titleType ': titleTypes)
  where
  buildObject'Arrows (AreObjectsCons IsObject' areObjects') f =
    HCons f (buildObject'Arrows @objectTypes @titleTypes areObjects' f)

mappedTitles :: HList [Literal "a", Literal "b", Literal "c"]
mappedTitles =
  let areObjects' = areObjects exampleObjects'
  in map
    areObjects'
    (buildObject'Arrows areObjects' title)
    exampleObjects'
