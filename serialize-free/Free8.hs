module Free8 where

import Free

data LineLength = Short | Medium | Long deriving (Show, Read, Eq, Enum, Bounded)

data Command a where
    GetLineLength :: Command LineLength
    PutLine :: String -> Command ()

deriving instance Show (Command a)

runCommand :: Command a -> IO a
runCommand GetLineLength = do
    line <- getLine
    return $ case length line of
        n | n < 10 -> Short
          | n < 20 -> Medium
          | otherwise -> Long
runCommand (PutLine line) = putStrLn line

data SomeCommand = forall a. (Show a, Enum a, Bounded a) => SomeCommand (Command a)

readCommand :: String -> Maybe (SomeCommand, String)
readCommand s = case lex s of
    [("GetLineLength", rest)] -> Just (SomeCommand GetLineLength, rest)
    [("PutLine", rest)] -> case lex rest of
        [(line, rest')] -> Just (SomeCommand (PutLine line), rest')
        _ -> Nothing
    _ -> Nothing

data Action a = forall r. (Enum r, Bounded r) => Action (Command r) (r -> a)

deriving instance Functor Action

instance (Show a) => Show (Action a) where
    show (Action command next) = show command ++ " " ++ show [ show (next r) | r <- [minBound .. maxBound] ]

type Program a = Free Action a

readProgram :: (Read a) => String -> Maybe (Program a)
readProgram s = case lex s of
    [("Pure", rest)] -> Just (Pure $ read rest)
    [("Free", rest)] -> case readCommand rest of
        Just (SomeCommand command, rest') -> case readPrograms rest' of
            Just nextPrograms -> Just (Free (Action command (\r -> nextPrograms !! fromEnum r)))
            Nothing -> Nothing
        Nothing -> Nothing
    _ -> Nothing

readPrograms :: (Read a) => String -> Maybe [Program a]
readPrograms s = case reads s of
    [(programs, "")] -> sequenceA $ map readProgram programs
    _ -> Nothing

runProgram :: Program a -> IO a
runProgram = foldFree interpret
  where
    interpret :: Action a -> IO a
    interpret (Action command f) = f <$> runCommand command

getLineLengthP :: Program LineLength
getLineLengthP = Free (Action GetLineLength Pure)

putLineP :: String -> Program ()
putLineP line = Free (Action (PutLine line) Pure)

program :: Program ()
program = do
    lineLength <- getLineLengthP
    case lineLength of
        Short -> putLineP "You entered a short line."
        Medium -> do putLineP "You entered a medium line."
                     nextLineLength <- getLineLengthP
                     putLineP $ "You entered a " ++ show nextLineLength ++ " line."
        Long -> putLineP "You entered a long line."
