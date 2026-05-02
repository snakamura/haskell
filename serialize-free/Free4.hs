module Free4 where

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

deriving instance Show SomeCommand

readCommand :: String -> Maybe (SomeCommand, String)
readCommand s = case head $ lex s of
    ("GetLineLength", rest) -> Just (SomeCommand GetLineLength, rest)
    ("PutLine", rest) -> let (param, rest') = head $ lex rest
                         in Just (SomeCommand (PutLine param), rest')
    _ -> Nothing

runSomeCommand :: SomeCommand -> IO String
runSomeCommand (SomeCommand command) = show <$> runCommand command

data Action a = forall r. (Enum r, Bounded r) => Action (Command r) (r -> a)

instance (Show a) => Show (Action a) where
    show (Action command next) = show command ++ " " ++ show [ show (next r) | r <- [minBound .. maxBound] ]

deriving instance Functor Action

type Program a = Free Action a

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

readProgram :: String -> Maybe (Program String)
readProgram s = case head $ lex s of
    ("Pure", rest) -> Just (Pure rest)
    ("Free", rest) -> case readCommand rest of
        Just (SomeCommand command, rest') -> case readPrograms rest' of
            Just nextPrograms -> Just (Free (Action command (\r -> nextPrograms !! fromEnum r)))
            Nothing -> Nothing
        Nothing -> Nothing
    _ -> Nothing

readPrograms :: String -> Maybe [Program String]
readPrograms s = case reads s of
    [(programs, "")] -> sequenceA $ map readProgram programs
    _ -> Nothing
