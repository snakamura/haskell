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
parse = fst . parser
    where
        parser = parseBranch |>>= \regex ->
                 empty |>>
                 always regex

parseBranch :: Parser String Regex
parseBranch =     empty |>>
                  always [[]]
              <|> parsePiece |>>= \ps ->
                  char '|' |>>
                  parseBranch |>>= \regex ->
                  always (ps:regex)
              <|> parsePiece |>>= \ps ->
                  always [ps]

parsePiece :: Parser String Seq
parsePiece =     parseGroup |>>= \g ->
                 parsePiece |>>= \ps ->
                 always (g:ps)
             <|> parseAtom |>>= \a ->
                 parsePiece |>>= \ps ->
                 always (a:ps)
             <|> always []

parseGroup :: Parser String Piece
parseGroup =     char '(' |>>
                 parseBranch |>>= \regex ->
                 char ')' |>>
                 char '?' |>>
                 always (Group regex, Optional)
             <|> char '(' |>>
                 parseBranch |>>= \regex ->
                 char ')' |>>
                 char '*' |>>
                 always (Group regex, Repeat)
             <|> char '(' |>>
                 parseBranch |>>= \regex ->
                 char ')' |>>
                 always (Group regex, None)

parseAtom :: Parser String Piece
parseAtom =     charOf isAtomChar |>>= \c ->
                char '?' |>>
                always (CharAtom c, Optional)
            <|> charOf isAtomChar |>>= \c ->
                char '*' |>>
                always (CharAtom c, Repeat  )
            <|> charOf isAtomChar |>>= \c ->
                always (CharAtom c, None    )

empty :: Parser String ()
empty [] = (Just (), [])
empty s  = (Nothing, s )

always :: b -> Parser a b
always x s = (Just x, s)

char :: Char -> Parser String Char
char c = charOf (==c)

charOf :: (Char -> Bool) -> Parser String Char
charOf f []                = (Nothing, []   )
charOf f (c:s) | f c       = (Just c,  s    )
               | otherwise = (Nothing, (c:s))

isAtomChar :: Char -> Bool
isAtomChar = flip notElem "|()*?"


type Parser a b = a -> (Maybe b, a)

infixl 1 <|>
(<|>) :: Parser a b -> Parser a b -> Parser a b
(<|>) parse1 parse2 = \s -> let (x, r) = parse1 s
                            in case x of
                                   Nothing -> parse2 s
                                   _       -> (x, r)

infixl 7 |>>=
(|>>=) :: Parser a b -> (b -> Parser a c) -> Parser a c
(|>>=) parser1 f = \s -> let (x, r) = parser1 s
                         in case x of
                                Nothing -> (Nothing, r)
                                Just v  -> f v r

infixl 7 |>>
(|>>) :: Parser a b -> Parser a c -> Parser a c
(|>>) parser1 parser2 = parser1 |>>= \_ -> parser2
