module Regex5 where

import Prelude hiding (seq)

data RSeq c = REmpty | RSeq c (RAlt c) deriving (Show)

newtype RAlt c = RAlt [RSeq c] deriving (Show)

rNever :: RAlt c
rNever = RAlt []

rEmpty :: RAlt c
rEmpty = RAlt [REmpty]

rSeq :: RAlt c -> RAlt c -> RAlt c
rSeq (RAlt seqs) alt = RAlt $ concat [rSeq' seq alt | seq <- seqs]
  where
    rSeq' :: RSeq c -> RAlt c -> [RSeq c]
    rSeq' REmpty (RAlt seqs') = seqs'
    rSeq' (RSeq c1 (RAlt seqs')) alt' = [RSeq c1 (RAlt (rSeq' seq' alt')) | seq' <- seqs']

rAlt :: RAlt c -> RAlt c -> RAlt c
rAlt (RAlt seqs1) (RAlt seqs2) = RAlt $ seqs1 <> seqs2

rMany :: RAlt c -> RAlt c
rMany r =
  let RAlt seqs = rSeq r (rMany r)
   in RAlt (REmpty : seqs)

matchAlt :: (c -> a -> [a]) -> RAlt c -> a -> [a]
matchAlt matchChar' (RAlt seqs) s = concat [matchSeq matchChar' seq s | seq <- seqs]

matchSeq :: (c -> a -> [a]) -> RSeq c -> a -> [a]
matchSeq _ REmpty s = [s]
matchSeq matchChar' (RSeq char alt) s = matchChar' char s >>= matchAlt matchChar' alt

newtype RChar = RChar Char deriving (Show)

type Regex = RAlt RChar

rChar :: Char -> Regex
rChar c = RAlt [RSeq (RChar c) rEmpty]

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
match r s = elem "" $ matchAlt matchChar r s

matchChar :: RChar -> String -> [String]
matchChar (RChar rc) (c : cs)
  | rc == c = [cs]
  | otherwise = []
matchChar _ [] = []
