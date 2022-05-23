module Main (main) where

import Data.Maybe (catMaybes)
import Item
    ( SomeItem(SomeItem)
    , SCat(..)
    , SSubCat(..)
    , buildItem
    , makeItem
    )

main :: IO ()
main = do
    let items :: [SomeItem]
        items = catMaybes [ SomeItem <$> buildItem SCat1 SSubCat2
                          , makeItem "Cat2" "SubCat3"
                          , makeItem "Cat2" "SubCat1"
                          ]
    mapM_ print items
