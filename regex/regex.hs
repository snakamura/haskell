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
        parser = branch >>= \regex ->
                 empty >>
                 return regex

branch :: Parser String Regex
branch =     (empty >>
              return [[]])
         <|> (piece >>= \ps ->
              char '|' >>
              branch >>= \regex ->
              return (ps:regex))
         <|> (piece >>= \ps ->
              return [ps])

piece :: Parser String Seq
piece =     (group >>= \g ->
             piece >>= \ps ->
             return (g:ps))
        <|> (atom >>= \a ->
             piece >>= \ps ->
             return (a:ps))
        <|>  return []

group :: Parser String Piece
group =     (char '(' >>
             branch >>= \regex ->
             char ')' >>
             char '?' >>
             return (Group regex, Optional))
        <|> (char '(' >>
             branch >>= \regex ->
             char ')' >>
             char '*' >>
             return (Group regex, Repeat))
        <|> (char '(' >>
             branch >>= \regex ->
             char ')' >>
             return (Group regex, None))

atom :: Parser String Piece
atom =     (charOf isAtomChar >>= \c ->
            char '?' >>
            return (CharAtom c, Optional))
       <|> (charOf isAtomChar >>= \c ->
            char '*' >>
            return (CharAtom c, Repeat  ))
       <|> (charOf isAtomChar >>= \c ->
            return (CharAtom c, None    ))

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
