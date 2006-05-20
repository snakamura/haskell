import Char
import System

data ParseError = ParseError {
    index :: Int,
    msg   :: String
}
    deriving Show

parseDecEither :: String -> Either ParseError Int
parseDecEither [] = Left $ ParseError 0 "Empty String"
parseDecEither s = parseDec' 0 s 0
 where
     parseDec' x (c:s) index = case parseDecCharEither c index of
                                   Right y -> let z = x*10 + y
                                              in if z < 0
                                                     then Left $ ParseError index "Overflow"
                                                     else parseDec' z s (index + 1)
                                   e       -> e
     parseDec' x [] _        = Right x

parseDecCharEither :: Char -> Int -> Either ParseError Int
parseDecCharEither c index = if isDigit c
                                 then Right $ fromEnum c - fromEnum '0'
                                 else Left $ ParseError index ("Invalid character: " ++ [c])

class Error e where
    fromString :: String -> e

instance Error ParseError where
    fromString = ParseError 0

class Monad m => MonadError e m | m -> e where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a

instance Error e => Monad (Either e) where
    return          = Right
    (Left  x) >>= _ = Left x
    (Right x) >>= f = f x
    fail            = Left . fromString

instance Error e => MonadError e (Either e) where
    throwError             = Left
    catchError (Right x) _ = Right x
    catchError (Left  e) f = f e

parseDecM :: MonadError ParseError m => String -> m Int
parseDecM [] = throwError $ ParseError 0 "Empty String"
parseDecM s = parseDec' 0 s 0
 where
     parseDec' x (c:s) index = do y <- parseDecCharM c index
                                  let z = x*10 + y
                                  if z < 0
                                      then throwError $ ParseError index "Overflow"
                                      else parseDec' z s (index + 1)
     parseDec' x [] _        = return x

parseDecCharM :: MonadError ParseError m => Char -> Int -> m Int
parseDecCharM c index = if isDigit c
                            then return $ fromEnum c - fromEnum '0'
                            else throwError $ ParseError index ("Invalid character: " ++ [c])

{-
main = do args <- getArgs
          let (Right x) = catchError (parse $ args !! 0) (return . show)
          putStrLn x
 where
     parse s = do x <- parseDecM s
                  return $ show x

main = do args <- getArgs
          case parseDecM (args !! 0) of
              Right x -> putStrLn $ show x
              Left  e -> putStrLn $ msg e

main = do args <- getArgs
          let s = args !! 0
              (Right x) = catchError (parseDecM s >>= return . show) (return . show)
          putStrLn x
-}

parse s = do let (Right x) = catchError (parse' s >>= return . show) (return . show)
             putStrLn x
 where
     parse' s = catchError (parseDecM s) handler
      where
          handler e = if index e == 0
                          then return 0
                          else throwError e
