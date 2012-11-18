import Control.Monad.Free (Free(..),
                           liftF)
import Prelude hiding (getChar,
                       putChar)
import qualified System.IO as IO
import Test.QuickCheck (quickCheck)

data Action a = GetChar (Char -> a)
              | PutChar Char a

instance Functor Action where
    fmap f (GetChar g) = GetChar (f . g)
    fmap f (PutChar c next) = PutChar c (f next)


getChar :: Free Action Char
getChar = liftF (GetChar id)

putChar :: Char -> Free Action ()
putChar c = liftF (PutChar c ())

echo :: Free Action ()
echo = getChar >>= putChar >> echo


runActionIO :: Free Action a -> IO a
runActionIO (Free (GetChar f)) = IO.getChar >>= runActionIO . f
runActionIO (Free (PutChar c next)) = IO.putChar c >> runActionIO next
runActionIO (Pure a) = return a


main :: IO ()
main = runActionIO echo


runActionPure :: Free Action a -> String -> String
runActionPure (Free (GetChar f)) "" = []
runActionPure (Free (GetChar f)) (c:cs) = runActionPure (f c) cs
runActionPure (Free (PutChar c next)) cs = c:runActionPure next cs
runActionPure (Pure a) _ = []


prop_echo :: String -> Bool
prop_echo s = runActionPure echo s == s

test :: IO ()
test = quickCheck prop_echo
