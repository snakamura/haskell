module Regex4 where

import Prelude hiding (seq)

newtype RChar = RChar Char deriving (Show)

data RSeq = REmpty | RSeq RChar RAlt deriving (Show)

newtype RAlt = RAlt [RSeq] deriving (Show)

type Regex = RAlt

rNever :: Regex
rNever = RAlt []

rEmpty :: Regex
rEmpty = RAlt [REmpty]

rChar :: Char -> Regex
rChar c = RAlt [RSeq (RChar c) rEmpty]

rSeq :: Regex -> Regex -> Regex
rSeq (RAlt alt1) alt2 = RAlt $ concat [f a1 alt2 | a1 <- alt1]
  where
    f :: RSeq -> RAlt -> [RSeq]
    f REmpty (RAlt alt) = alt
    f (RSeq c1 (RAlt alt1')) alt2' = [RSeq c1 (RAlt (f a1 alt2')) | a1 <- alt1']

rAlt :: Regex -> Regex -> Regex
rAlt (RAlt alt1) (RAlt alt2) = RAlt $ alt1 <> alt2

rMany :: Regex -> Regex
rMany r =
  let RAlt alt = rSeq r (rMany r)
   in RAlt (REmpty : alt)

regex0, regex1, regex2, regex3, regex4, regex5, regex6, regex7, regex8 :: Regex
regex0 = rNever
regex1 = rEmpty -- //
regex2 = rChar 'a' -- /a/
regex3 = rChar 'a' `rSeq` rChar 'b' -- /ab/
regex4 = rChar 'a' `rAlt` rChar 'b' -- /a|b/
regex5 = rEmpty `rAlt` rChar 'a' -- /|a/
regex6 = rMany (rChar 'a') -- /a*/
regex7 = (rChar 'a' `rSeq` rChar 'b') `rAlt` (rMany (rChar 'c' `rSeq` rChar 'd') `rSeq` rChar 'e') -- /ab|(cd)*e/
regex8 = rMany (rChar 'a') `rSeq` rChar 'a' -- /a*a/

match :: Regex -> String -> Bool
match r s = elem "" $ matchAlt r s

matchAlt :: RAlt -> String -> [String]
matchAlt (RAlt alt) s = concatMap (flip matchSeq s) alt

matchSeq :: RSeq -> String -> [String]
matchSeq REmpty s = [s]
matchSeq (RSeq char alt) s = matchChar char s >>= matchAlt alt

matchChar :: RChar -> String -> [String]
matchChar (RChar rc) (c : cs)
  | rc == c = [cs]
  | otherwise = []
matchChar _ [] = []
