module Expr where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text (Text, pack)
import Util

data Expr a where
  LiteralInt :: Int -> Expr Int
  Add :: Expr Int -> Expr Int -> Expr Int

deriving stock instance Show (Expr a)

int :: Parser (Expr Int)
int = add <* endOfInput

add :: Parser (Expr Int)
add = chainl1 literalInt (many space *> char '+' *> many space *> pure Add)

literalInt :: Parser (Expr Int)
literalInt = LiteralInt . read @Int <$> many1 digit

eval :: Expr a -> a
eval (LiteralInt n) = n
eval (Add lhs rhs) = eval lhs + eval rhs

format :: Expr a -> Text
format (LiteralInt n) = pack $ show n
format (Add lhs rhs) = format lhs <> " + " <> format rhs

formatRPN :: Expr a -> Text
formatRPN (LiteralInt n) = pack $ show n
formatRPN (Add lhs rhs) = formatRPN lhs <> " " <> formatRPN rhs <> " +"

expr :: Expr Int
expr = case parseOnly int "10 + 20 + 30 + 40" of
  Right e -> e
  Left e -> error $ show e

v1 :: Int
v1 = eval expr

v2 :: Text
v2 = format expr

v3 :: Text
v3 = formatRPN expr
