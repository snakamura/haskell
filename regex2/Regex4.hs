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
rSeq (RAlt seqs) alt = RAlt $ concat [rSeq' seq alt | seq <- seqs]
  where
    rSeq' :: RSeq -> RAlt -> [RSeq]
    rSeq' REmpty (RAlt seqs') = seqs'
    rSeq' (RSeq c1 (RAlt seqs')) alt' = [RSeq c1 (RAlt (rSeq' seq' alt')) | seq' <- seqs']

rAlt :: Regex -> Regex -> Regex
rAlt (RAlt seqs1) (RAlt seqs2) = RAlt $ seqs1 <> seqs2

rMany :: Regex -> Regex
rMany r =
  let RAlt seqs = rSeq r (rMany r)
   in RAlt (REmpty : seqs)

regex0, regex1, regex2, regex3, regex4, regex5, regex6, regex7, regex8, regex9, regex10 :: Regex
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
regex10 = rMany (rChar 'a' `rAlt` rChar 'b') `rSeq` rMany (rChar 'a' `rAlt` rChar 'c') -- (a|b)*(a|b|c)*

match :: Regex -> String -> Bool
match r s = elem "" $ matchAlt r s

matchAlt :: RAlt -> String -> [String]
matchAlt (RAlt seqs) s = concat [matchSeq seq s | seq <- seqs]

matchSeq :: RSeq -> String -> [String]
matchSeq REmpty s = [s]
matchSeq (RSeq char alt) s = matchChar char s >>= matchAlt alt

matchChar :: RChar -> String -> [String]
matchChar (RChar rc) (c : cs)
  | rc == c = [cs]
  | otherwise = []
matchChar _ [] = []
