{-# LANGUAGE DataKinds,
             TypeApplications
#-}

module Main (main) where

import Item
    ( Item(Item)
    , SomeItem(SomeItem)
    , Cat(..)
    , SubCat(..)
    )

main :: IO ()
main = do
    let item1 = Item @'Cat1 @'SubCat2
        item2 = Item @'Cat2 @'SubCat3
        -- item3 = Item @'Cat2 @'SubCat1
    print item1
    print item2
    mapM_ print [ SomeItem item1
                , SomeItem item2
                ]
