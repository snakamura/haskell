{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Regex where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Regex.Posix

qr = QuasiQuoter parseRegex undefined

--parseRegex s = sigE (appE (varE 'makeRegex) (stringE s)) [t|Regex|]
parseRegex s = sigE (appE (varE 'makeRegex) (stringE s)) (conT ''Regex)
