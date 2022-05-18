{-# LANGUAGE DataKinds,
             GADTs,
             PolyKinds,
             ScopedTypeVariables,
             StandaloneKindSignatures,
             TypeApplications,
             TypeFamilies,
             TypeOperators
#-}

module Item
    ( Item(Item)
    , SomeItem(SomeItem)
    , type Cat(..)
    , SCat(..)
    , type SubCat(..)
    , SSubCat(..)
    , buildItem
    ) where

import Data.Kind (Constraint, Type)
import Data.Type.Equality (type (:~:)(Refl), TestEquality, testEquality)
import Unsafe.Coerce (unsafeCoerce)

data Cat = Cat1 | Cat2

type SCat :: Cat -> Type
data SCat t where
    SCat1 :: SCat 'Cat1
    SCat2 :: SCat 'Cat2

instance Show (SCat cat) where
    show SCat1 = "Cat1"
    show SCat2 = "Cat2"

type SCatI :: Cat -> Constraint
class SCatI cat where
    singCat :: SCat cat
instance SCatI 'Cat1 where
    singCat = SCat1
instance SCatI 'Cat2 where
    singCat = SCat2


data SubCat = SubCat1 | SubCat2 | SubCat3

type SSubCat :: SubCat -> Type
data SSubCat subCat where
    SSubCat1 :: SSubCat 'SubCat1
    SSubCat2 :: SSubCat 'SubCat2
    SSubCat3 :: SSubCat 'SubCat3

instance Show (SSubCat subCat) where
    show SSubCat1 = "SubCat1"
    show SSubCat2 = "SubCat2"
    show SSubCat3 = "SubCat3"

type SSubCatI :: SubCat -> Constraint
class SSubCatI subCat where
    singSubCat :: SSubCat subCat
instance SSubCatI 'SubCat1 where
    singSubCat = SSubCat1
instance SSubCatI 'SubCat2 where
    singSubCat = SSubCat2
instance SSubCatI 'SubCat3 where
    singSubCat = SSubCat3

instance TestEquality SSubCat where
    testEquality SSubCat1 SSubCat1 = Just Refl
    testEquality SSubCat2 SSubCat2 = Just Refl
    testEquality SSubCat3 SSubCat3 = Just Refl
    testEquality _ _ = Nothing


type ValidSubCats :: Cat -> [SubCat]
type family ValidSubCats cat where
    ValidSubCats 'Cat1 = '[ 'SubCat1, 'SubCat2 ]
    ValidSubCats 'Cat2 = '[ 'SubCat2, 'SubCat3 ]

validSSubCats :: SCat cat -> HL SSubCat (ValidSubCats cat)
validSSubCats SCat1 = SSubCat1 `HCons` SSubCat2 `HCons` HNil
validSSubCats SCat2 = SSubCat2 `HCons` SSubCat3 `HCons` HNil

type Item :: Cat -> SubCat -> Type
data Item cat subCat where
    Item :: forall cat subCat. ToConstraint (OneOf subCat (ValidSubCats cat)) => Item cat subCat
instance (SCatI cat, SSubCatI subCat) => Show (Item cat subCat) where
    show _ = "Item " <> show (singCat @cat) <> " " <> show (singSubCat @subCat)

data SomeItem where
    SomeItem :: (SCatI cat, SSubCatI subCat) => Item cat subCat -> SomeItem
instance Show SomeItem where
    show (SomeItem item) = show item

buildItem :: SCat cat -> SSubCat subCat -> Maybe (Item cat subCat)
buildItem cat subCat = case oneOf subCat (validSSubCats cat) of
                           Just Refl -> Just Item
                           Nothing -> Nothing


type OneOf :: k -> [k] -> Bool
type family OneOf t ts where
    OneOf t (t ': _) = 'True
    OneOf t (_ ': ts) = OneOf t ts
    OneOf _ _ = 'False

type ToConstraint :: Bool -> Constraint
type family ToConstraint b where
    ToConstraint 'True = ()
    ToConstraint 'False = ('True ~ 'False)

type HL :: (k -> Type) -> [k] -> Type
data HL f xs where
    HCons :: f x -> HL f xs -> HL f (x ': xs)
    HNil :: HL f '[]
infixr 5 `HCons`

oneOf :: TestEquality f => f x -> HL f xs -> Maybe (OneOf x xs :~: 'True)
oneOf x (HCons y ys) = case testEquality x y of
                           Just Refl -> Just Refl
                           Nothing -> unsafeCoerce $ oneOf x ys
oneOf _ _ = Nothing
