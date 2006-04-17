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

parse :: String -> Regex
parse s = let (regex, rest) = parseBranch s
          in case rest of
              [] -> regex
              _  -> error "Invalid pattern."

parseBranch :: String -> (Regex, String)
parseBranch = uncurry parseNext . parsePiece
    where parseNext seq []      = ([seq], [])
          parseNext seq ('|':s) = uncurry makeBranch $ parseBranch s
              where
                  makeBranch regex s = (seq:regex, s)
          parseNext seq s       = ([seq], s)

parsePiece :: String -> (Seq, String)
parsePiece = uncurry parseNext . parseOnePiece
    where
        parseOnePiece s = let (regex, rest) = parseGroup s
                          in case regex of
                                 Nothing -> parseAtom s
                                 _       -> (regex, rest)
        parseNext Nothing rest       = ([],  rest)
        parseNext (Just p) []        = ([p], []  )
        parseNext (Just p) rest      = uncurry makeSeq $ parsePiece rest
            where
                makeSeq [] s = ([p],     s)
                makeSeq ps s = (p:ps,    s)

parseGroup :: String -> (Maybe Piece, String)
parseGroup ('(':s) = uncurry makeGroup $ parseBranch s
    where
        makeGroup regex (')':'?':s) = (Just (Group regex, Optional), s    )
        makeGroup regex (')':'*':s) = (Just (Group regex, Repeat),   s    )
        makeGroup regex (')':s)     = (Just (Group regex, None),     s    )
        makeGroup _ _               = (Nothing,                      '(':s)
parseGroup s       = (Nothing, s)

parseAtom :: String -> (Maybe Piece, String)
parseAtom []                      = (Nothing, [])
parseAtom cs@(c:s) | isAtomChar c = makeAtom s
                   | otherwise    = (Nothing, cs)
    where
        makeAtom ('?':s) = (Just (CharAtom c, Optional), s)
        makeAtom ('*':s) = (Just (CharAtom c, Repeat),   s)
        makeAtom s       = (Just (CharAtom c, None),     s)

isAtomChar :: Char -> Bool
isAtomChar = flip notElem "|()*?"
