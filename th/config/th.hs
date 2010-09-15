{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.Maybe
import Language.Haskell.TH

import Key

$(sequence [dataD (cxt [])
                  (mkName "Config")
                  []
                  [recC (mkName "Config") (map (\(n, t, _, _) -> varStrictType (mkName n) (strictType isStrict t)) keys)]
                  [''Show]])

main = do config <- liftM parse $ readFile "config"
          print config

parse :: String -> Config
parse s = let values = catMaybes $ map parseLine $ lines s
          in $(foldl appE (conE (mkName "Config")) (map (\(n, _, f, d) -> [|fromMaybe $d $ lookup n values >>= $f|]) keys))

parseLine :: String -> Maybe (String, String)
parseLine ""      = Nothing
parseLine ('#':_) = Nothing
parseLine s       = case break (== '=') s of
                      ("", _) -> Nothing
                      (n, v)  -> Just (n, tail v)
