import Char
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
branch =     do s <- seq
                char '|'
                b <- branch
                return $ s:b
         <|> do s <- seq
                return [s]

seq :: Parser String Seq
seq =     do p <- piece
             s <- seq
             return $ p:s
      <|> do p <- piece
             return [p]
      <|> return []

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

empty :: Parser String ()
empty = Parser empty'
    where
        empty' [] = Just ((), [])
        empty' s  = Nothing

always :: b -> Parser a b
always = Parser . always'
    where
        always' x s = Just (x, s)

char :: Char -> Parser String Char
char c = charOf (==c)

charOf :: (Char -> Bool) -> Parser String Char
charOf = Parser . charOf'
    where
        charOf' f []                = Nothing
        charOf' f (c:s) | f c       = Just (c, s)
                        | otherwise = Nothing

isAtomChar :: Char -> Bool
isAtomChar = flip notElem "|()*?"


newtype Parser a b = Parser { runParser :: a -> Maybe (b, a) }

instance Monad (Parser a)
    where
        (>>=)  = (|>>=)
        return = always
        fail _ = Parser $ \s -> Nothing

instance MonadPlus (Parser a)
    where
        mzero = Parser $ \s -> Nothing
        mplus = (<|>)

infixr 1 <|>
(<|>) :: Parser a b -> Parser a b -> Parser a b
(<|>) (Parser parse1) (Parser parse2) = Parser $ \s -> let r = parse1 s
                                                       in case r of
                                                              Nothing -> parse2 s
                                                              _       -> r

infixl 1 |>>=
(|>>=) :: Parser a b -> (b -> Parser a c) -> Parser a c
(|>>=) (Parser parser1) f = Parser $ \s -> case parser1 s of
                                               Nothing     -> Nothing
                                               Just (v, s) -> runParser (f v) $ s

infixl 1 |>>
(|>>) :: Parser a b -> Parser a c -> Parser a c
(|>>) parser1 parser2 = parser1 |>>= \_ -> parser2
