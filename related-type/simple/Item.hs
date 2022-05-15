module Item
    ( Item
    , cat
    , subCat
    , Cat(..)
    , SubCat(..)
    , buildItem
    , makeItem
    ) where

import Control.Monad (join)
import Text.Read (readMaybe)

data Cat = Cat1 | Cat2 deriving (Show, Read, Eq)

data SubCat = SubCat1 | SubCat2 | SubCat3 deriving (Show, Read, Eq)

data Item = Item
    { cat :: Cat
    , subCat :: SubCat
    }
instance Show Item where
    show (Item cat subCat) = "Item " <> show cat <> " " <> show subCat

validSubCats :: Cat -> [SubCat]
validSubCats Cat1 = [SubCat1, SubCat2]
validSubCats Cat2 = [SubCat2, SubCat3]

buildItem :: Cat -> SubCat -> Maybe Item
buildItem cat subCat | subCat `elem` validSubCats cat = Just $ Item cat subCat
buildItem _ _ = Nothing

makeItem :: String -> String -> Maybe Item
makeItem cat subCat = join $ buildItem <$> readMaybe cat <*> readMaybe subCat
