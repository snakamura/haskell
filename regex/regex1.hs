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
parse s = case parseBranch s of
               (Nothing, _ ) -> Nothing
               (Just b,  []) -> Just b
               _             -> Nothing

parseBranch :: String -> (Maybe Regex, String)
parseBranch s = case parseSeq s of
                    (Nothing,  rs    ) -> (Nothing, rs)
                    (Just seq, '|':rs) -> case parseBranch rs of
                                              (Nothing, rs') -> (Nothing,       rs')
                                              (Just b', rs') -> (Just (seq:b'), rs')
                    (Just seq, rs    ) -> (Just [seq], rs)

parseSeq :: String -> (Maybe Seq, String)
parseSeq s = case parsePiece s of
                 (Nothing, rs) -> (Just [],  rs)
                 (Just p,  []) -> (Just [p], [])
                 (Just p,  rs) -> case parseSeq rs of
                                      (Nothing,  rs') -> (Nothing,      rs')
                                      (Just seq, rs') -> (Just (p:seq), rs')

parsePiece :: String -> (Maybe Piece, String)
parsePiece s = case parseAtom s of
                   (Nothing, rs) -> (Nothing, rs)
                   (Just a,  rs) -> case parseQuantifier rs of
                                        (Nothing, rs') -> (Nothing,     rs')
                                        (Just q,  rs') -> (Just (a, q), rs')

parseAtom :: String -> (Maybe Atom, String)
parseAtom s = case parseGroup s of
                  (Nothing, _) -> parseCharAtom s
                  g            -> g

parseGroup :: String -> (Maybe Atom, String)
parseGroup ('(':s) = case parseBranch s of
                         (Nothing, rs    ) -> (Nothing,        rs)
                         (Just b,  ')':rs) -> (Just (Group b), rs)
                         (_,       rs    ) -> (Nothing,        rs)
parseGroup s       = (Nothing, s)

parseCharAtom :: String -> (Maybe Atom, String)
parseCharAtom (c:s) | isAtomChar c = (Just (CharAtom c), s)
                    | otherwise    = (Nothing, c:s)
parseCharAtom []                   = (Nothing, [])

parseQuantifier :: String -> (Maybe Quantifier, String)
parseQuantifier ('*':s) = (Just Repeat,   s)
parseQuantifier ('?':s) = (Just Optional, s)
parseQuantifier s       = (Just None,     s)

isAtomChar :: Char -> Bool
isAtomChar = flip notElem "|()*?"
