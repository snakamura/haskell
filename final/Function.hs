module Function where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text (Text, pack)
import Util

int :: (Parse a) => Parser a
int = add <* endOfInput

add :: (Parse a) => Parser a
add = chainl1 literalInt (many space *> char '+' *> many space *> pure addP)

literalInt :: (Parse a) => Parser a
literalInt = literalIntP . read <$> many1 digit

class Parse a where
  literalIntP :: Int -> a
  addP :: a -> a -> a

instance Parse Int where
  literalIntP = id
  addP = (+)

instance Parse Text where
  literalIntP = pack . show
  addP lhs rhs = lhs <> " + " <> rhs

newtype RPN = RPN Text

instance Parse RPN where
  literalIntP = RPN . pack . show
  addP (RPN lhs) (RPN rhs) = RPN $ lhs <> " " <> rhs <> " +"

v :: (Parse a) => a
v = case parseOnly int "10 + 20 + 30 + 40" of
  Right e -> e
  Left _ -> error "Never happens"

v1 :: Int
v1 = v

v2 :: Text
v2 = v

v3 :: Text
v3 =
  let RPN t = v
   in t
