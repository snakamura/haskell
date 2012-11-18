import Control.Monad ((>=>))
import Prelude hiding (getChar,
                       putChar)
import qualified System.IO as IO
import Test.QuickCheck (quickCheck)

data Action a = GetChar (Char -> Action a)
              | PutChar Char (Action a)
              | Return a

instance Monad Action where
    return = Return
    GetChar g >>= f = GetChar (g >=> f)
    PutChar c next >>= f = PutChar c (next >>= f)
    Return x >>= f = f x


getChar :: Action Char
getChar = GetChar Return

putChar :: Char -> Action ()
putChar c = PutChar c (Return ())

echo :: Action ()
echo = getChar >>= putChar >> echo


runActionIO :: Action a -> IO a
runActionIO (GetChar f) = IO.getChar >>= runActionIO . f
runActionIO (PutChar c next) = IO.putChar c >> runActionIO next
runActionIO (Return a) = return a


main :: IO ()
main = runActionIO echo


runActionPure :: Action a -> String -> String
runActionPure (GetChar f) "" = []
runActionPure (GetChar f) (c:cs) = runActionPure (f c) cs
runActionPure (PutChar c next) cs = c:runActionPure next cs
runActionPure (Return a) _ = []


prop_echo :: String -> Bool
prop_echo s = runActionPure echo s == s

test :: IO ()
test = quickCheck prop_echo
