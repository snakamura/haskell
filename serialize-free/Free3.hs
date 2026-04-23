module Free3 where

import Data.Functor.Coyoneda
import Data.List
import Text.Read
import Free

data Command a where
    GetLine :: Command String
    PutLine :: String -> Command ()

deriving instance Show (Command a)

runCommand :: Command a -> IO a
runCommand GetLine = getLine
runCommand (PutLine line) = putStrLn line

data SomeCommand = forall a. (Show a) => SomeCommand (Command a)

deriving instance Show SomeCommand

readCommand :: String -> Maybe SomeCommand
readCommand "GetLine" = Just (SomeCommand GetLine)
readCommand s | "PutLine " `isPrefixOf` s
              , Just line <- readMaybe $ drop (length "PutLine ") s =
    Just (SomeCommand (PutLine line))
readCommand _ = Nothing

runSomeCommand :: SomeCommand -> IO String
runSomeCommand (SomeCommand command) = show <$> runCommand command

type Program a = Free (Coyoneda Command) a

getLineP :: Program String
getLineP = Free (Coyoneda Pure GetLine)

putLineP :: String -> Program ()
putLineP line = Free (Coyoneda Pure (PutLine line))

program :: Program ()
program = do
    line <- getLineP
    putLineP ("You entered: " ++ line)
