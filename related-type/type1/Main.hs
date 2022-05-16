{-# LANGUAGE DataKinds,
             TypeApplications
#-}

module Main (main) where

import Item
    ( Item(Item)
    , Cat(..)
    , SubCat(..)
    )

main :: IO ()
main = do
    let item1 = Item @'Cat1 @'SubCat2
        item2 = Item :: Item Cat2 SubCat3
        -- item3 = Item @'Cat2 @'SubCat1
    print "Item"
