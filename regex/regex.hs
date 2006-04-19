import Control.Monad
import Prelude hiding (seq)

type Regex = [Seq]

type Seq = [Piece]

type Piece = (Atom, Quantifier)

data Atom = CharAtom Char
          | Group Regex
    deriving Show

data Quantifier = None
                | Optional
                | Repeat
    deriving Show

parse :: String -> Maybe Regex
parse s = (runParser parser) s >>= return . fst
    where
        parser = do regex <- branch
                    empty
                    return regex

branch :: Parser String Regex
branch = sepBy1 seq '|'

seq :: Parser String Seq
seq = many piece

piece :: Parser String Piece
piece = do a <- atom
           q <- quantifier
           return (a, q)

atom :: Parser String Atom
atom =     charAtom
       <|> group

charAtom :: Parser String Atom
charAtom = do c <- charOf isAtomChar
              return $ CharAtom c

group :: Parser String Atom
group = do char '('
           b <- branch
           char ')'
           return $ Group b

quantifier :: Parser String Quantifier
quantifier =     do char '*'
                    return Repeat
             <|> do char '?'
                    return Optional
             <|> return None

empty :: Parser [a] ()
empty = Parser empty'
    where
        empty' [] = Just ((), [])
        empty' _  = Nothing

always :: b -> Parser a b
always = Parser . always'
    where
        always' x s = Just (x, s)

char :: Char -> Parser String Char
char = token

charOf :: (Char -> Bool) -> Parser String Char
charOf = tokenOf

token :: Eq a => a -> Parser [a] a
token c = tokenOf (==c)

tokenOf :: (a -> Bool) -> Parser [a] a
tokenOf = Parser . token'
    where
        token' f []                = Nothing
        token' f (c:s) | f c       = Just (c, s)
                       | otherwise = Nothing

many :: Parser a b -> Parser a [b]
many parser =     do r <- parser
                     m <- many parser
                     return $ r:m
              <|> do r <- parser
                     return [r]
              <|> do return []

sepBy1 :: Eq a => Parser [a] b -> a -> Parser [a] [b]
sepBy1 parser sep =     do r <- parser
                           token sep
                           m <- sepBy1 parser sep
                           return $ r:m
                    <|> do r <- parser
                           return [r]

isAtomChar :: Char -> Bool
isAtomChar = flip notElem "|()*?"


newtype Parser a b = Parser { runParser :: a -> Maybe (b, a) }

instance Monad (Parser a)
    where
        (>>=)  = (|>>=)
        return = always
        fail _ = Parser $ \_ -> Nothing

instance MonadPlus (Parser a)
    where
        mzero = Parser $ \_ -> Nothing
        mplus = (<|>)

infixr 1 <|>
(<|>) :: Parser a b -> Parser a b -> Parser a b
(<|>) (Parser parse1) (Parser parse2) = Parser $ \s -> case parse1 s of
                                                           Nothing -> parse2 s
                                                           r       -> r

infixl 1 |>>=
(|>>=) :: Parser a b -> (b -> Parser a c) -> Parser a c
(|>>=) (Parser parser1) f = Parser $ \s -> case parser1 s of
                                               Nothing     -> Nothing
                                               Just (v, s) -> runParser (f v) $ s

infixl 1 |>>
(|>>) :: Parser a b -> Parser a c -> Parser a c
(|>>) parser1 parser2 = parser1 |>>= \_ -> parser2
