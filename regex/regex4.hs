import Prelude hiding (seq)

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
        parser = do b <- branch
                    empty
                    return b

branch :: Parser String Branch
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
atom =     group
       <|> charAtom

group :: Parser String Atom
group = do char '('
           b <- branch
           char ')'
           return $ Group b

charAtom :: Parser String Atom
charAtom = do c <- charOf isAtomChar
              return $ CharAtom c

quantifier :: Parser String Quantifier
quantifier =     do char '*'
                    return Repeat
             <|> do char '?'
                    return Optional
             <|> return None

char :: Char -> Parser String Char
char c = charOf (==c)

charOf :: (Char -> Bool) -> Parser String Char
charOf f = Parser $ \s -> charOf' f s
    where
        charOf' f (c:s) | f c       = Just (c, s)
                        | otherwise = Nothing
        charOf' _ []                = Nothing

empty :: Parser [a] ()
empty = Parser $ empty'
    where
        empty' [] = Just ((), [])
        empty' _  = Nothing

always :: b -> Parser a b
always x = Parser $ \s -> Just (x, s)

isAtomChar :: Char -> Bool
isAtomChar = flip notElem "|()*?"


newtype Parser a b = Parser { runParser :: a -> Maybe (b, a) }

instance Monad (Parser a)
    where
        (>>=) = (|>>=)
        return = always
        fail _ = Parser $ \_ -> Nothing

(<|>) :: Parser a b -> Parser a b -> Parser a b
(<|>) (Parser p1) (Parser p2) = Parser $ \s -> case p1 s of
                                                   Nothing -> p2 s
                                                   r       -> r

(|>>=) :: Parser a b -> (b -> Parser a c) -> Parser a c
(|>>=) (Parser p) f = Parser $ \s -> case p s of
                                         Nothing     -> Nothing
                                         Just (v, rs)-> runParser (f v) $ rs

(|>>) :: Parser a b -> Parser a c -> Parser a c
(|>>) p1 p2 = p1 |>>= \_ -> p2
