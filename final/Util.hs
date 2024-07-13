module Util where

import Control.Applicative
import Data.Attoparsec.Types

chainl1 :: Parser i a -> Parser i (a -> a -> a) -> Parser i a
chainl1 termP opP = do
  term <- termP
  rest term
  where
    rest term =
      do
        op <- opP
        nextTerm <- termP
        rest (op term nextTerm)
        <|> pure term
