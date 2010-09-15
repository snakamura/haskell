{-# LANGUAGE TemplateHaskell #-}

module Key where

import Data.List.Split
import Data.Maybe
import Language.Haskell.TH

keys :: [(String, TypeQ, ExpQ, ExpQ)]
keys = [("name",  [t|String|],        [|Just . id|],         [|""|]     ),
        ("value", [t|Int|],           [|maybeRead|],         [|0|]      ),
        ("list",  [t|[String]|],      [|Just . sepBy "," |], [|[]|]     ),
        ("tuple", [t|(String, Int)|], [|maybeRead|],         [|("", 0)|])]

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
                [(v, [])] -> Just v
                _         -> Nothing
