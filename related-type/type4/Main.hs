module Main (main) where

import Data.Maybe(catMaybes)
import Item
    ( SomeItem(SomeItem)
    , SCat(..)
    , SSubCat(..)
    , buildItem
    )

main :: IO ()
main = do
    let item1 = buildItem SCat1 SSubCat2
        item2 = buildItem SCat2 SSubCat3
        item3 = buildItem SCat2 SSubCat1
    print item1
    print item2
    print item3
    mapM_ print $ catMaybes [ SomeItem <$> item1
                            , SomeItem <$> item2
                            , SomeItem <$> item3
                            ]
