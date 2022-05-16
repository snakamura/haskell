{-# LANGUAGE DataKinds,
             ExplicitForAll,
             GADTs,
             PolyKinds,
             StandaloneKindSignatures,
             TypeFamilies,
             TypeOperators
#-}

module Item
    ( Item(Item)
    , type Cat(..)
    , type SubCat(..)
    ) where

import Data.Kind (Constraint, Type)

data Cat = Cat1 | Cat2
data SubCat = SubCat1 | SubCat2 | SubCat3

type ValidSubCats :: Cat -> [SubCat]
type family ValidSubCats cat where
    ValidSubCats 'Cat1 = '[ 'SubCat1, 'SubCat2 ]
    ValidSubCats 'Cat2 = '[ 'SubCat2, 'SubCat3 ]

type Item :: Cat -> SubCat -> Type
data Item cat subCat where
    Item :: forall cat subCat. ToConstraint (OneOf subCat (ValidSubCats cat)) => Item cat subCat


type OneOf :: k -> [k] -> Bool
type family OneOf t ts where
    OneOf t (t ': _) = 'True
    OneOf t (_ ': ts) = OneOf t ts
    OneOf _ _ = 'False

type ToConstraint :: Bool -> Constraint
type family ToConstraint b where
    ToConstraint 'True = ()
    ToConstraint 'False = ('True ~ 'False)
