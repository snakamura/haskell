module Eval where

import Control.Applicative
import Data.Attoparsec.Text
import Util

int :: Parser Int
int = add <* endOfInput

add :: Parser Int
add = chainl1 literalInt (many space *> char '+' *> many space *> pure (+))

literalInt :: Parser Int
literalInt = read @Int <$> many1 digit

v1 :: Int
v1 = case parseOnly int "10 + 20 + 30 + 40" of
  Right e -> e
  Left _ -> error "Never happens"
