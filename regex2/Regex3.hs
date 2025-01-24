module Regex3 where

import Data.Maybe (mapMaybe, listToMaybe)
import Data.Monoid (Sum)

data Regex a
  = Never
  | Empty a
  | Char Char a
  | Seq (Regex a) (Regex a)
  | Alt (Regex a) (Regex a)
  deriving (Show)

many :: Monoid a => Regex a -> Regex a
many r = Empty mempty `Alt` (r `Seq` many r)

empty :: Regex (Sum Int)
empty = Empty 0

char :: Char -> Regex (Sum Int)
char c = Char c 1

regex0, regex1, regex2, regex3, regex4, regex5, regex6, regex7, regex8 :: Regex (Sum Int)
regex0 = Never
regex1 = empty -- //
regex2 = char 'a' -- /a/
regex3 = char 'a' `Seq` char 'b' -- /ab/
regex4 = char 'a' `Alt` char 'b' -- /a|b/
regex5 = empty `Alt` char 'a' -- /|a/
regex6 = many (char 'a') -- /a*/
regex7 = (char 'a' `Seq` char 'b') `Alt` (many (char 'c' `Seq` char 'd') `Seq` char 'e') -- /ab|(cd)*e/
regex8 = many (char 'a') `Seq` char 'a' -- /a*a/

empty' :: Regex ()
empty' = Empty ()

char' :: Char -> Regex ()
char' c = Char c ()

regex1', regex2', regex3', regex4', regex5', regex6', regex7', regex8' :: Regex ()
regex1' = empty' -- //
regex2' = char' 'a' -- /a/
regex3' = char' 'a' `Seq` char' 'b' -- /ab/
regex4' = char' 'a' `Alt` char' 'b' -- /a|b/
regex5' = empty' `Alt` char' 'a' -- /|a/
regex6' = many (char' 'a') -- /a*/
regex7' = (char' 'a' `Seq` char' 'b') `Alt` (many (char' 'c' `Seq` char' 'd') `Seq` char' 'e') -- /ab|(cd)*e/
regex8' = many (char' 'a') `Seq` char' 'a' -- /a*a/

empty'' :: Regex String
empty'' = Empty ""

char'' :: Char -> Regex String
char'' c = Char c [c]

regex1'', regex2'', regex3'', regex4'', regex5'', regex6'', regex7'', regex8'' :: Regex String
regex1'' = empty'' -- //
regex2'' = char'' 'a' -- /a/
regex3'' = char'' 'a' `Seq` char'' 'b' -- /ab/
regex4'' = char'' 'a' `Alt` char'' 'b' -- /a|b/
regex5'' = empty'' `Alt` char'' 'a' -- /|a/
regex6'' = many (char'' 'a') -- /a*/
regex7'' = (char'' 'a' `Seq` char'' 'b') `Alt` (many (char'' 'c' `Seq` char'' 'd') `Seq` char'' 'e') -- /ab|(cd)*e/
regex8'' = many (char'' 'a') `Seq` char'' 'a' -- /a*a/

match :: Monoid a => Regex a -> String -> Maybe a
match r s = listToMaybe $ mapMaybe f (match' r s)
  where
    f (n, "") = Just n
    f _ = Nothing

match' :: Monoid a => Regex a -> String -> [(a, String)]
match' Never _ = []
match' (Empty a) s = [(a, s)]
match' (Char rc a) (c : cs)
  | rc == c = [(a, cs)]
  | otherwise = []
match' (Char _ _) "" = []
match' (Seq r1 r2) s = do
  (a1, s1) <- match' r1 s
  (a2, s2) <- match' r2 s1
  pure (a1 <> a2, s2)
match' (Alt r1 r2) s = match' r1 s <> match' r2 s
