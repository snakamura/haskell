import Char
import Prelude hiding (catch)
import System

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


--parseDecM :: (Monad m, MonadError String m) => String -> m Int
parseDecM [] = throw "Empty String"
parseDecM s = parseDec' 0 s
 where
     parseDec' x (c:s) = do y <- parseDecCharM c
                            let z = x*10 + y
                            if z < 0
                                then throw "Overflow"
                                else parseDec' z s
     parseDec' x []    = return x

--parseDecCharM :: (Monad m, MonadError String m) => Char -> m Int
parseDecCharM c = if isDigit c
                      then return $ fromEnum c - fromEnum '0'
                      else throw $ "Invalid character: " ++ [c]

{-
instance MonadError a Maybe where
    throw _ = Nothing

parseDecMaybeM :: String -> Maybe Int
parseDecMaybeM = parseDecM
-}

class Error a where
    fromString :: String -> a

instance Error String where
    fromString = id

class Monad m => MonadError e m | m -> e where
    throw :: e -> m a
    catch :: m a -> (e -> m a) -> m a

instance Error e => Monad (Either e) where
    return          = Right
    (Left  x) >>= _ = Left x
    (Right x) >>= f = f x
    fail            = Left . fromString

instance Error e => MonadError e (Either e) where
    throw             = Left
    catch (Right x) _ = Right x
    catch (Left  e) f = f e


parseDecEitherM :: String -> Either String Int
parseDecEitherM = parseDecM

{-
main = do args <- getArgs
          case parseDecM $ args !! 0 of
              Right x -> putStrLn $ show x
              Left e  -> putStrLn e
-}
main = do args <- getArgs
          let (Right x) = catch (parse $ args !! 0) (return . id)
          putStrLn x
 where
     parse s = do x <- parseDecM s
                  return $ show x
