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
    , type SubCat(..)
    ) where

import Data.Kind (Constraint, Type)

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


type ValidSubCats :: Cat -> [SubCat]
type family ValidSubCats cat where
    ValidSubCats 'Cat1 = '[ 'SubCat1, 'SubCat2 ]
    ValidSubCats 'Cat2 = '[ 'SubCat2, 'SubCat3 ]

type Item :: Cat -> SubCat -> Type
data Item cat subCat where
    Item :: forall cat subCat. ToConstraint (OneOf subCat (ValidSubCats cat)) => Item cat subCat
instance (SCatI cat, SSubCatI subCat) => Show (Item cat subCat) where
    show _ = "Item " <> show (singCat @cat) <> " " <> show (singSubCat @subCat)

data SomeItem where
    SomeItem :: (SCatI cat, SSubCatI subCat) => Item cat subCat -> SomeItem
instance Show SomeItem where
    show (SomeItem item) = show item


type OneOf :: k -> [k] -> Bool
type family OneOf t ts where
    OneOf t (t ': _) = 'True
    OneOf t (_ ': ts) = OneOf t ts
    OneOf _ _ = 'False

type ToConstraint :: Bool -> Constraint
type family ToConstraint b where
    ToConstraint 'True = ()
    ToConstraint 'False = ('True ~ 'False)
