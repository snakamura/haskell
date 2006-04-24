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
parse s = parse' s >>= return . Regex . fst
    where
        parse' = parseBranch |>>= (\b ->
                 parseEmpty |>>
                 (always b))

parseBranch :: Parser String Branch
parseBranch =     (parseSeq |>>= (\s ->
                   parseChar '|' |>>
                   parseBranch |>>= (\b ->
                   always $ s:b)))
              <|> (parseSeq |>>= (\s ->
                   always [s]))

parseSeq :: Parser String Seq
parseSeq =     (parsePiece |>>= (\p ->
                parseSeq |>>= (\s ->
                always $ p:s)))
           <|> (parsePiece |>>= (\p ->
                always [p]))
           <|> always []

parsePiece :: Parser String Piece
parsePiece = parseAtom |>>= (\a ->
             parseQuantifier |>>= (\q ->
             always (a, q)))

parseAtom :: Parser String Atom
parseAtom =     parseGroup
            <|> parseCharAtom

parseGroup :: Parser String Atom
parseGroup = parseChar '(' |>>
             parseBranch |>>= (\b ->
             parseChar ')' |>>
             always (Group b))

parseCharAtom :: Parser String Atom
parseCharAtom = parseCharOf isAtomChar |>>= (\c ->
                always $ CharAtom c)

parseQuantifier :: Parser String Quantifier
parseQuantifier =     (parseChar '*' |>>
                       always Repeat)
                  <|> (parseChar '?' |>>
                       always Optional)
                  <|> always None

parseChar :: Char -> Parser String Char
parseChar c = parseCharOf (==c)

parseCharOf :: (Char -> Bool) -> Parser String Char
parseCharOf f (c:s) | f c       = Just (c, s)
                    | otherwise = Nothing
parseCharOf _ []                = Nothing

parseEmpty :: Parser [a] ()
parseEmpty [] = Just ((), [])
parseEmpty _  = Nothing

always :: b -> Parser a b
always x = \s -> Just (x, s)

isAtomChar :: Char -> Bool
isAtomChar = flip notElem "|()*?"


type Parser a b = a -> Maybe (b, a)

(<|>) :: Parser a b -> Parser a b -> Parser a b
(<|>) p1 p2 = \s -> case p1 s of
                        Nothing -> p2 s
                        r       -> r

(|>>=) :: Parser a b -> (b -> Parser a c) -> Parser a c
(|>>=) p f = \s -> case p s of
                       Nothing     -> Nothing
                       Just (v, rs)-> (f v) rs

(|>>) :: Parser a b -> Parser a c -> Parser a c
(|>>) p1 p2 = p1 |>>= \_ -> p2
