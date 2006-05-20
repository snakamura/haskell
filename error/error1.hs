import Char

parseDecMaybe :: String -> Maybe Int
parseDecMaybe [] = Nothing
parseDecMaybe s = parseDec' 0 s
 where
     parseDec' x (c:s) = case parseDecCharMaybe c of
                             Just y -> let z = x*10 + y
                                       in if z < 0
                                              then Nothing
                                              else parseDec' z s
                             Nothing -> Nothing
     parseDec' x []    = Just x

parseDecCharMaybe :: Char -> Maybe Int
parseDecCharMaybe c = if isDigit c
                          then Just $ fromEnum c - fromEnum '0'
                          else Nothing

parseDecEither :: String -> Either String Int
parseDecEither [] = Left "Empty String"
parseDecEither s = parseDec' 0 s
 where
     parseDec' x (c:s) = case parseDecCharEither c of
                             Right y -> let z = x*10 + y
                                        in if z < 0
                                               then Left "Overflow"
                                               else parseDec' z s
                             e       -> e
     parseDec' x []    = Right x

parseDecCharEither :: Char -> Either String Int
parseDecCharEither c = if isDigit c
                           then Right $ fromEnum c - fromEnum '0'
                           else Left $ "Invalid character: " ++ [c]

parseDecM :: Monad m => String -> m Int
parseDecM [] = fail "Empty String"
parseDecM s = parseDec' 0 s
 where
     parseDec' x (c:s) = do y <- parseDecCharM c
                            let z = x*10 + y
                            if z < 0
                                then fail "Overflow"
                                else parseDec' z s
     parseDec' x []    = return x

parseDecCharM :: Monad m => Char -> m Int
parseDecCharM c = if isDigit c
                      then return $ fromEnum c - fromEnum '0'
                      else fail $ "Invalid character: " ++ [c]

instance Error e => Monad (Either e) where
    return          = Right
    (Left  x) >>= _ = Left x
    (Right x) >>= f = f x
    fail            = Left . fromString

class Error a where
    fromString :: String -> a

instance Error String where
    fromString = id

