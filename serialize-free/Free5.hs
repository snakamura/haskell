module Free5 where

import Data.Functor.Coyoneda
import Free

data Command a where
    GetLine :: Command String
    PutLine :: String -> Command ()

runCommand :: Command a -> IO a
runCommand GetLine = getLine
runCommand (PutLine line) = putStrLn line

type Program a = Free (Coyoneda Command) a

runProgram :: Program a -> IO a
runProgram = foldFree (lowerCoyoneda . hoistCoyoneda runCommand)

getLineP :: Program String
getLineP = Free (Coyoneda Pure GetLine)

putLineP :: String -> Program ()
putLineP line = Free (Coyoneda Pure (PutLine line))

program :: Program ()
program = do
    putLineP "Enter: "
    line <- getLineP
    putLineP ("You entered: " ++ line)
