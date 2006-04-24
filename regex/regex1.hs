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
parse s = case parseBranch s of
               Nothing      -> Nothing
               Just (b, []) -> Just (Regex b)
               _            -> Nothing

parseBranch :: String -> Maybe (Branch, String)
parseBranch s = case parseSeq s of
                    Nothing            -> Nothing
                    Just (seq, '|':rs) -> case parseBranch rs of
                                              Nothing        -> Nothing
                                              Just (b', rs') -> Just (seq:b', rs')
                    Just (seq, rs    ) -> Just ([seq], rs)

parseSeq :: String -> Maybe (Seq, String)
parseSeq s = case parsePiece s of
                 Nothing       -> Just ([], s)
                 Just (p,  []) -> Just ([p], [])
                 Just (p,  rs) -> case parseSeq rs of
                                      Nothing         -> Nothing
                                      Just (seq, rs') -> Just (p:seq, rs')

parsePiece :: String -> Maybe (Piece, String)
parsePiece s = case parseAtom s of
                   Nothing      -> Nothing
                   Just (a, rs) -> case parseQuantifier rs of
                                        Nothing       -> Nothing
                                        Just (q, rs') -> Just ((a, q), rs')

parseAtom :: String -> Maybe (Atom, String)
parseAtom s = case parseGroup s of
                  Nothing -> parseCharAtom s
                  g       -> g

parseGroup :: String -> Maybe (Atom, String)
parseGroup ('(':s) = case parseBranch s of
                         Nothing          -> Nothing
                         Just (b, ')':rs) -> Just (Group b, rs)
                         Just (_, rs    ) -> Nothing
parseGroup s       = Nothing

parseCharAtom :: String -> Maybe (Atom, String)
parseCharAtom (c:s) | isAtomChar c = Just (CharAtom c, s)
                    | otherwise    = Nothing
parseCharAtom []                   = Nothing

parseQuantifier :: String -> Maybe (Quantifier, String)
parseQuantifier ('*':s) = Just (Repeat,   s)
parseQuantifier ('?':s) = Just (Optional, s)
parseQuantifier s       = Just (None,     s)

isAtomChar :: Char -> Bool
isAtomChar = flip notElem "|()*?"
