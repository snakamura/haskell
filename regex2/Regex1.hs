module Regex1 where

data Regex
  = Never
  | Empty
  | Char Char
  | Seq Regex Regex
  | Alt Regex Regex
  deriving (Show)

many :: Regex -> Regex
many r = Empty `Alt` (r `Seq` many r)

regex0, regex1, regex2, regex3, regex4, regex5, regex6, regex7, regex8, regex9 :: Regex
regex0 = Never
regex1 = Empty -- //
regex2 = Char 'a' -- /a/
regex3 = Char 'a' `Seq` Char 'b' -- /ab/
regex4 = Char 'a' `Alt` Char 'b' -- /a|b/
regex5 = Empty `Alt` Char 'a' -- /|a/
regex6 = many (Char 'a') -- /a*/
regex7 = (Char 'a' `Seq` Char 'b') `Alt` (many (Char 'c' `Seq` Char 'd') `Seq` Char 'e') -- /ab|(cd)*e/
regex8 = many (Char 'a') `Seq` Char 'a' -- /a*a/
regex9 = ((Char 'a' `Alt` Char 'b') `Seq` Char 'c') `Alt` (Char 'd' `Seq` (Char 'e' `Alt` Char 'f') `Seq` Char 'g') -- (a|b)c|d(e|f)g

match :: Regex -> String -> Bool
match r s = elem "" $ match' r s

match' :: Regex -> String -> [String]
match' Never _ = []
match' Empty s = [s]
match' (Char rc) (c : cs)
  | rc == c = [cs]
  | otherwise = []
match' (Char _) "" = []
match' (Seq r1 r2) s = match' r1 s >>= match' r2
match' (Alt r1 r2) s = match' r1 s <> match' r2 s


{-
module Regex14 where

data Regex
  = Never
  | Empty
  | Char Char
  | Seq Regex Regex
  | Alt Regex Regex
  | Many Regex
  deriving (Show)

regex0, regex1, regex2, regex3, regex4, regex5, regex6, regex7, regex8 :: Regex
regex0 = Never
regex1 = Empty -- //
regex2 = Char 'a' -- /a/
regex3 = Char 'a' `Seq` Char 'b' -- /ab/
regex4 = Char 'a' `Alt` Char 'b' -- /a|b/
regex5 = Empty `Alt` Char 'a' -- /|a/
regex6 = Many (Char 'a') -- /a*/
regex7 = (Char 'a' `Seq` Char 'b') `Alt` (Many (Char 'c' `Seq` Char 'd') `Seq` Char 'e') -- /ab|(cd)*e/
regex8 = Many (Char 'a') `Seq` Char 'a' -- /a*a/

match :: Regex -> String -> Bool
match r s = elem "" $ match' r s

match' :: Regex -> String -> [String]
match' Never _ = []
match' Empty s = [s]
match' (Char rc) (c : cs)
  | rc == c = [cs]
  | otherwise = []
match' (Char _) "" = []
match' (Seq r1 r2) s = match' r1 s >>= match' r2
match' (Alt r1 r2) s = match' r1 s <> match' r2 s
match' many@(Many r) s = (match' r s >>= match' many) <> [s]
-}
