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
parse = fst . runParser parser
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
atom =     atomChar
       <|> group

atomChar :: Parser String Atom
atomChar = do c <- charOf isAtomChar
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
        empty' [] = (Just (), [])
        empty' s  = (Nothing, s )

always :: b -> Parser a b
always = Parser . always'
    where
        always' x s = (Just x, s)

char :: Char -> Parser String Char
char c = charOf (==c)

charOf :: (Char -> Bool) -> Parser String Char
charOf = Parser . charOf'
    where
        charOf' f []                = (Nothing, []   )
        charOf' f (c:s) | f c       = (Just c,  s    )
                        | otherwise = (Nothing, (c:s))

isAtomChar :: Char -> Bool
isAtomChar = flip notElem "|()*?"


newtype Parser a b = Parser { runParser :: a -> (Maybe b, a) }

instance Monad (Parser a)
    where
        (>>=) = (|>>=)
        return = always

instance MonadPlus (Parser a)
    where
        mzero = Parser $ \s -> (Nothing, s)
        mplus = (<|>)

infixr 1 <|>
(<|>) :: Parser a b -> Parser a b -> Parser a b
(<|>) (Parser parse1) (Parser parse2) = Parser $ \s -> let (x, r) = parse1 s
                                                       in case x of
                                                              Nothing -> parse2 s
                                                              _       -> (x, r)

infixl 1 |>>=
(|>>=) :: Parser a b -> (b -> Parser a c) -> Parser a c
(|>>=) (Parser parser1) f = Parser $ \s -> let (x, r) = parser1 s
                                           in case x of
                                                  Nothing -> (Nothing, r)
                                                  Just v  -> runParser (f v) $ r

infixl 1 |>>
(|>>) :: Parser a b -> Parser a c -> Parser a c
(|>>) parser1 parser2 = parser1 |>>= \_ -> parser2
