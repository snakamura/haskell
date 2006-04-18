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
                 always regex

branch :: Parser String Regex
branch =     (empty >>
              always [[]])
         <|> (piece >>= \ps ->
              char '|' >>
              branch >>= \regex ->
              always (ps:regex))
         <|> (piece >>= \ps ->
              always [ps])

piece :: Parser String Seq
piece =     (group >>= \g ->
             piece >>= \ps ->
             always (g:ps))
        <|> (atom >>= \a ->
             piece >>= \ps ->
             always (a:ps))
        <|>  always []

group :: Parser String Piece
group =     (char '(' >>
             branch >>= \regex ->
             char ')' >>
             char '?' >>
             always (Group regex, Optional))
        <|> (char '(' >>
             branch >>= \regex ->
             char ')' >>
             char '*' >>
             always (Group regex, Repeat))
        <|> (char '(' >>
             branch >>= \regex ->
             char ')' >>
             always (Group regex, None))

atom :: Parser String Piece
atom =     (charOf isAtomChar >>= \c ->
            char '?' >>
            always (CharAtom c, Optional))
       <|> (charOf isAtomChar >>= \c ->
            char '*' >>
            always (CharAtom c, Repeat  ))
       <|> (charOf isAtomChar >>= \c ->
            always (CharAtom c, None    ))

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

infixl 9 |>>=
(|>>=) :: Parser a b -> (b -> Parser a c) -> Parser a c
(|>>=) (Parser parser1) f = Parser $ \s -> let (x, r) = parser1 s
                                           in case x of
                                                  Nothing -> (Nothing, r)
                                                  Just v  -> runParser (f v) $ r

infixl 9 |>>
(|>>) :: Parser a b -> Parser a c -> Parser a c
(|>>) parser1 parser2 = parser1 |>>= \_ -> parser2
