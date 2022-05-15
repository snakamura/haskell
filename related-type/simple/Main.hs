module Main (main) where

import Data.Maybe (catMaybes)
import Item
    ( Item
    , Cat(..)
    , SubCat(..)
    , buildItem
    , makeItem
    )

main :: IO ()
main = do
    let items :: [Item]
        items = catMaybes [ buildItem Cat1 SubCat2
                          , makeItem "Cat2" "SubCat3"
                          , makeItem "Cat2" "SubCat1"
                          ]
    mapM_ print items
