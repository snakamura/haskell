{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module QQ where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

q = QuasiQuoter stringE (litP . stringL)
