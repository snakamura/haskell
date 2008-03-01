import Control.Monad
import Data.Char
import Data.List
import System.IO

value = do
    names <- liftM (sort . parse) $ readFile "22.txt"
    return $ sum $ zipWith (*) [1..] $ map score names

parse :: String -> [String]
parse s = read $ "[" ++ s ++ "]"
{-
parse = words . map v
 where
     v c | isUpper c = c
         | otherwise = ' '
-}

score = sum . map f
 where
     f c = fromEnum c - fromEnum 'A' + 1
