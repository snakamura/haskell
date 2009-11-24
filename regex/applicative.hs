module Main where

import Control.Applicative
import Control.Arrow (first)
import Prelude hiding (seq)


newtype Parser input output = Parser { runParser :: input -> Maybe (output, input) }

instance Functor (Parser input) where
  fmap f parser = Parser $ \i -> first f <$> runParser parser i

instance Applicative (Parser input) where
  pure v = Parser $ \i -> pure (v, i)
  f <*> v = Parser $ \i -> do (g, r) <- runParser f i 
                              (o, r') <- runParser v r 
                              return (g o, r')

instance Alternative (Parser input) where
  empty = Parser $ const empty
  l <|> r = Parser $ \i -> runParser l i <|> runParser r i


token :: Eq a => a -> Parser [a] a
token c = tokenOf (== c)

tokenOf :: (a -> Bool) -> Parser [a] a
tokenOf p = Parser tokenOf'
  where
    tokenOf' (c:cs) | p c = pure (c, cs)
    tokenOf' _            = empty

sepBy1 :: Eq a => Parser [a] b -> a -> Parser [a] [b]
sepBy1 parser sep = some
  where
    some = many <|> pure <$> parser
    many = (:) <$> (parser <* token sep) <*> some

end :: Parser [a] ()
end = Parser end'
  where
    end' [] = pure ((), [])
    end' _  = empty



newtype Regex = Regex Branch
    deriving Show

type Branch = [Seq]

type Seq = [Piece]

type Piece = (Atom, Quantifier)

data Atom = CharAtom Char
          | Group Branch
    deriving Show

data Quantifier = None
                | Optional
                | Repeat
    deriving Show


parse :: String -> Maybe Regex
parse s = (runParser parser) s >>= return . Regex . fst
  where
    parser = branch <* end

branch :: Parser String Branch
branch = sepBy1 seq '|'

seq :: Parser String Seq
seq = many piece

piece :: Parser String Piece
piece = (,) <$> atom <*> quantifier

atom :: Parser String Atom
atom = charAtom <|> group

charAtom :: Parser String Atom
charAtom = CharAtom <$> charOf isAtomChar

group :: Parser String Atom
group = Group <$> (char '(' *> branch <* char ')')

quantifier :: Parser String Quantifier
quantifier = Repeat <$ char '*' <|> Optional <$ char '?' <|> pure None

char :: Char -> Parser String Char
char = token

charOf :: (Char -> Bool) -> Parser String Char
charOf = tokenOf

isAtomChar :: Char -> Bool
isAtomChar = flip notElem "|()*?"
