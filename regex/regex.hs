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
parse s = case parseBranch s of
              (Nothing,    _ ) -> Nothing
              (Just regex, []) -> Just regex
              _                -> Nothing

parseBranch :: Parser String Regex
parseBranch = uncurry parseNext . parsePiece
    where parseNext Nothing s          = (Nothing,    s )
          parseNext (Just seq) ('|':s) = uncurry makeBranch $ parseBranch s
              where
                  makeBranch Nothing      s = (Nothing,          s)
                  makeBranch (Just regex) s = (Just (seq:regex), s)
          parseNext (Just seq) s       = (Just [seq],  s)

parsePiece :: Parser String Seq
parsePiece = uncurry parseNext . parseOnePiece
    where
        parseOnePiece = parseGroup <|> parseAtom
        parseNext Nothing rest    = (Nothing,  rest)
        parseNext (Just [] ) rest = (Just [],  rest)
        parseNext (Just [p]) []   = (Just [p], []  )
        parseNext (Just [p]) rest = uncurry makeSeq $ parsePiece rest
            where
                makeSeq Nothing   s = (Nothing,     s)
                makeSeq (Just ps) s = (Just (p:ps), s)

parseGroup :: Parser String [Piece]
parseGroup ('(':s) = uncurry makeGroup $ parseBranch s
    where
        makeGroup (Just regex) (')':'?':s) = (Just [(Group regex, Optional)], s    )
        makeGroup (Just regex) (')':'*':s) = (Just [(Group regex, Repeat)],   s    )
        makeGroup (Just regex) (')':s)     = (Just [(Group regex, None)],     s    )
        makeGroup _            _           = (Nothing,                        '(':s)
parseGroup s       = (Nothing, s)

parseAtom :: Parser String [Piece]
{-
parseAtom []                      = (Just [], [])
parseAtom cs@(c:s) | isAtomChar c = makeAtom s
                   | otherwise    = (Just [], cs)
    where
        makeAtom ('?':s) = (Just [(CharAtom c, Optional)], s)
        makeAtom ('*':s) = (Just [(CharAtom c, Repeat)],   s)
        makeAtom s       = (Just [(CharAtom c, None)],     s)
-}
{-
parseAtom =     empty
            <|> (charOf isAtomChar |>>= char '?') (\c _ -> [(CharAtom c, Optional)])
            <|> (charOf isAtomChar |>>= char '*') (\c _ -> [(CharAtom c, Repeat  )])
            <|> charOf isAtomChar |> (\c -> [(CharAtom c, None    )])
-}
{-
parseAtom =     empty
            <|> (charOf isAtomChar |>>= \c -> char '?' |> (\_ -> [(CharAtom c, Optional)]))
            <|> (charOf isAtomChar |>>= \c -> char '*' |> (\_ -> [(CharAtom c, Repeat  )]))
            <|> (charOf isAtomChar                     |> (\c -> [(CharAtom c, None    )]))
-}
parseAtom =     empty
            <|> (charOf isAtomChar |>>= \c -> char '?' |>>= (\_ -> always [(CharAtom c, Optional)]))
            <|> (charOf isAtomChar |>>= \c -> char '*' |>>= (\_ -> always [(CharAtom c, Repeat  )]))
            <|> (charOf isAtomChar                     |>>= (\c -> always [(CharAtom c, None    )]))

empty :: Parser String [a]
empty [] = (Just [], [])
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

infixl 5 <|>
(<|>) :: Parser a b -> Parser a b -> Parser a b
(<|>) parse1 parse2 = \s -> let (x, r) = parse1 s
                            in case x of
                                   Nothing -> parse2 s
                                   _       -> (x, r)

{-
infixl 6 |>>=
(|>>=) :: Parser a b -> Parser a c -> (b -> c -> d) -> Parser a d
(|>>=) parser1 parser2 = \f s -> let (x, r) = parser1 s
                                 in case x of
                                        Nothing -> (Nothing, r)
                                        Just v  -> let (x', r') = parser2 r
                                                   in case x' of
                                                          Nothing -> (Nothing,       r')
                                                          Just v' -> (Just $ f v v', r')

infixl 6 |>>
(|>>) :: Parser a b -> Parser a c -> Parser a c
(|>>) parser1 parser2 = (|>>=) parser1 parser2 (flip const)
-}

infixl 7 |>>=
(|>>=) :: Parser a b -> (b -> Parser a c) -> Parser a c
(|>>=) parser1 f = \s -> let (x, r) = parser1 s
                         in case x of
                                Nothing -> (Nothing, r)
                                Just v  -> f v r

{-
infixl 6 |>
(|>) :: Parser a b -> (b -> c) -> Parser a c
(|>) parser f = \s -> let (x, r) = parser s
                      in case x of
                             Nothing -> (Nothing,    r)
                             Just v  -> (Just $ f v, r)
-}
