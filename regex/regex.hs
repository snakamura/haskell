import Char

data Regex = Branch Regex Regex
           | Seq Regex Regex
           | Atom Char Quantifier
           | Group Regex Quantifier
           | Empty
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
parseBranch = uncurry makeBranch . parsePiece
    where makeBranch regex []      = (regex, [])
          makeBranch regex ('|':s) = let (regex', rest') = parseBranch s
                                     in (Branch regex regex', rest')
          makeBranch regex s       = (regex, s)

parsePiece :: String -> (Regex, String)
parsePiece = uncurry parseNext . parseOnePiece
    where
        parseOnePiece s = let (regex, rest) = parseGroup s
                          in case regex of
                                 Empty -> parseAtom s
                                 _     -> (regex, rest)
        parseNext Empty rest         = (Empty, rest)
        parseNext regex []           = (regex, []  )
        parseNext regex rest@('|':_) = (regex, rest)
        parseNext regex rest         = let (regex', rest') = parsePiece rest
                                       in case regex' of
                                              Empty -> (regex, rest')
                                              _     -> (Seq regex regex', rest')

parseGroup :: String -> (Regex, String)
parseGroup ('(':s) = uncurry makeGroup $ parseBranch s
    where
        makeGroup regex (')':'?':s) = (Group regex Optional, s)
        makeGroup regex (')':'*':s) = (Group regex Repeat,   s)
        makeGroup regex (')':s)     = (Group regex None,     s)
        makeGroup _ _               = error "Invalid pattern."
parseGroup s       = (Empty, s)

parseAtom :: String -> (Regex, String)
parseAtom []                      = (Empty, [])
parseAtom cs@(c:s) | isAtomChar c = makeAtom s
                   | otherwise    = (Empty, cs)
    where
        makeAtom ('?':s) = (Atom c Optional, s)
        makeAtom ('*':s) = (Atom c Repeat,   s)
        makeAtom s       = (Atom c None,     s)

isAtomChar :: Char -> Bool
isAtomChar = flip notElem "|()*?"
