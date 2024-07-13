module Format where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text (Text, pack)
import Util

int :: Parser Text
int = add <* endOfInput

add :: Parser Text
add = chainl1 literalInt (many space *> char '+' *> many space *> pure (\l r -> l <> " + " <> r))

literalInt :: Parser Text
literalInt = pack <$> many1 digit

v1 :: Text
v1 = case parseOnly int "10 + 20 + 30 + 40" of
  Right e -> e
  Left _ -> error "Never happens"
