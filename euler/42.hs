import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.IO

value = do values <- liftM (map wordValue . parse) $ readFile "42.txt"
           return $ length $ filter isTriangle values

parse s = read $ "[" ++ s ++ "]"

wordValue s = sum $ map charValue s

charValue c = ord c - ord 'A' + 1

triangle n = n*(n + 1) `div` 2

triangles = map triangle [1..]

isTriangle n = let x = fromJust $ find (\v -> v >= n) triangles
               in x == n
