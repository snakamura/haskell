module Free7 where

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

data Action a = forall r. (Enum r, Bounded r) => Action (Command r) (r -> a)

deriving instance Functor Action

instance (Show a) => Show (Action a) where
    show (Action command next) = show command ++ " " ++ show [ show (next r) | r <- [minBound .. maxBound] ]

type Program a = Free Action a

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
