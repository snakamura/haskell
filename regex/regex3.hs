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
parse s = (runParser parse') s >>= return . Regex . fst
    where
        parse' = parseBranch >>= (\b ->
                 parseEmpty >>
                 (return b))

parseBranch :: Parser String Branch
parseBranch =     (parseSeq >>= (\s ->
                   parseChar '|' >>
                   parseBranch >>= (\b ->
                   return $ s:b)))
              <|> (parseSeq >>= (\s ->
                   return [s]))

parseSeq :: Parser String Seq
parseSeq =     (parsePiece >>= (\p ->
                parseSeq >>= (\s ->
                return $ p:s)))
           <|> (parsePiece >>= (\p ->
                return [p]))
           <|> return []

parsePiece :: Parser String Piece
parsePiece = parseAtom >>= (\a ->
             parseQuantifier >>= (\q ->
             return (a, q)))

parseAtom :: Parser String Atom
parseAtom =     parseGroup
            <|> parseCharAtom

parseGroup :: Parser String Atom
parseGroup = parseChar '(' >>
             parseBranch >>= (\b ->
             parseChar ')' >>
             return (Group b))

parseCharAtom :: Parser String Atom
parseCharAtom = parseCharOf isAtomChar >>= (\c ->
                return $ CharAtom c)

parseQuantifier :: Parser String Quantifier
parseQuantifier =     (parseChar '*' >>
                       return Repeat)
                  <|> (parseChar '?' >>
                       return Optional)
                  <|> return None

parseChar :: Char -> Parser String Char
parseChar c = parseCharOf (==c)

parseCharOf :: (Char -> Bool) -> Parser String Char
parseCharOf f = Parser $ \s -> parseCharOf' f s
    where
        parseCharOf' f (c:s) | f c       = Just (c, s)
                             | otherwise = Nothing
        parseCharOf' _ []                = Nothing

parseEmpty :: Parser [a] ()
parseEmpty = Parser $ parseEmpty'
    where
        parseEmpty' [] = Just ((), [])
        parseEmpty' _  = Nothing

always :: b -> Parser a b
always x = Parser $ \s -> Just (x, s)

isAtomChar :: Char -> Bool
isAtomChar = flip notElem "|()*?"


newtype Parser a b = Parser { runParser :: a -> Maybe (b, a) }

instance Monad (Parser a)
    where
        (>>=) = (|>>=)
        return = always
        fail _ = Parser $ \_ -> Nothing

(<|>) :: Parser a b -> Parser a b -> Parser a b
(<|>) (Parser p1) (Parser p2) = Parser $ \s -> case p1 s of
                                                   Nothing -> p2 s
                                                   r       -> r

(|>>=) :: Parser a b -> (b -> Parser a c) -> Parser a c
(|>>=) (Parser p) f = Parser $ \s -> case p s of
                                         Nothing     -> Nothing
                                         Just (v, rs)-> runParser (f v) $ rs

(|>>) :: Parser a b -> Parser a c -> Parser a c
(|>>) p1 p2 = p1 |>>= \_ -> p2
