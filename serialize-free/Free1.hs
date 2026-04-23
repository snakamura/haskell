module Free1 where

import Data.Functor.Coyoneda
import Data.List
import Text.Read

data Command a where
    GetLine :: Command String
    PutLine :: String -> Command ()

deriving instance Show (Command a)

runCommand :: Command a -> IO a
runCommand GetLine = getLine
runCommand (PutLine line) = putStrLn line

data SomeCommand = forall a. SomeCommand (Command a)

deriving instance Show SomeCommand

readCommand :: String -> Maybe SomeCommand
readCommand "GetLine" = Just (SomeCommand GetLine)
readCommand s | "PutLine " `isPrefixOf` s
              , Just line <- readMaybe $ drop (length "PutLine ") s =
    Just (SomeCommand (PutLine line))
readCommand _ = Nothing
