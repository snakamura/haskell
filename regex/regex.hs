import Char

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
branch =     do empty
                return [[]]
         <|> do ps <- piece
                char '|'
                regex <- branch
                return (ps:regex)
         <|> do ps <- piece
                return [ps]

piece :: Parser String Seq
piece =     do g <- group
               ps <- piece
               return (g:ps)
        <|> do a <- atom
               ps <- piece
               return (a:ps)
        <|> return []

group :: Parser String Piece
group =     do char '('
               regex <- branch
               char ')'
               char '?'
               return (Group regex, Optional)
        <|> do char '('
               regex <- branch
               char ')'
               char '*'
               return (Group regex, Repeat)
        <|> do char '('
               regex <- branch
               char ')'
               return (Group regex, None)

atom :: Parser String Piece
atom =     do c <- charOf isAtomChar
              char '?'
              return (CharAtom c, Optional)
       <|> do c <- charOf isAtomChar
              char '*'
              return (CharAtom c, Repeat)
       <|> do c <- charOf isAtomChar
              return (CharAtom c, None)

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
