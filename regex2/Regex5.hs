module Regex5 where

import Prelude hiding (seq)

data RSeq c = REmpty | RSeq c (RAlt c) deriving (Show)

newtype RAlt c = RAlt [RSeq c] deriving (Show)

rNever :: RAlt c
rNever = RAlt []

rEmpty :: RAlt c
rEmpty = RAlt [REmpty]

rSeq :: RAlt c -> RAlt c -> RAlt c
rSeq (RAlt alt1) alt2 = RAlt $ concat [f a1 alt2 | a1 <- alt1]
  where
    f :: RSeq c -> RAlt c -> [RSeq c]
    f REmpty (RAlt alt) = alt
    f (RSeq c1 (RAlt alt1')) alt2' = [RSeq c1 (RAlt (f a1 alt2')) | a1 <- alt1']

rAlt :: RAlt c -> RAlt c -> RAlt c
rAlt (RAlt alt1) (RAlt alt2) = RAlt $ alt1 <> alt2

rMany :: RAlt c -> RAlt c
rMany r =
  let RAlt alt = rSeq r (rMany r)
   in RAlt (REmpty : alt)

matchAlt :: (c -> a -> [a]) -> RAlt c -> a -> [a]
matchAlt matchChar' (RAlt alt) s = concatMap (flip (matchSeq matchChar') s) alt

matchSeq :: (c -> a -> [a]) -> RSeq c -> a -> [a]
matchSeq _ REmpty s = [s]
matchSeq matchChar' (RSeq char alt) s = matchChar' char s >>= matchAlt matchChar' alt

newtype RChar = RChar Char deriving (Show)

type Regex = RAlt RChar

rChar :: Char -> Regex
rChar c = RAlt [RSeq (RChar c) rEmpty]

regex0, regex1, regex2, regex3, regex4, regex5, regex6, regex7, regex8, regex9 :: Regex
regex0 = rNever
regex1 = rEmpty -- //
regex2 = rChar 'a' -- /a/
regex3 = rChar 'a' `rSeq` rChar 'b' -- /ab/
regex4 = rChar 'a' `rAlt` rChar 'b' -- /a|b/
regex5 = rEmpty `rAlt` rChar 'a' -- /|a/
regex6 = rMany (rChar 'a') -- /a*/
regex7 = (rChar 'a' `rSeq` rChar 'b') `rAlt` (rMany (rChar 'c' `rSeq` rChar 'd') `rSeq` rChar 'e') -- /ab|(cd)*e/
regex8 = rMany (rChar 'a') `rSeq` rChar 'a' -- /a*a/
regex9 = ((rChar 'a' `rAlt` rChar 'b') `rSeq` rChar 'c') `rAlt` (rChar 'd' `rSeq` (rChar 'e' `rAlt` rChar 'f') `rSeq` rChar 'g') -- (a|b)c|d(e|f)g

match :: Regex -> String -> Bool
match r s = elem "" $ matchAlt matchChar r s

matchChar :: RChar -> String -> [String]
matchChar (RChar rc) (c : cs)
  | rc == c = [cs]
  | otherwise = []
matchChar _ [] = []
