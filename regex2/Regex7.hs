module Regex7 where

import Data.Maybe (listToMaybe, mapMaybe)
import Data.Monoid (Sum)
import Prelude hiding (seq)

data RChar a = RChar Char a

data RSeq a = REmpty a | RSeq (RChar a) (RAlt a)

newtype RAlt a = RAlt [RSeq a]

type Regex = RAlt

rNever :: RAlt a
rNever = RAlt []

rEmpty :: (Monoid a) => RAlt a
rEmpty = RAlt [REmpty mempty]

rSeq :: RAlt a -> RAlt a -> RAlt a
rSeq (RAlt seqs) alt = RAlt $ concat [rSeq' seq alt | seq <- seqs]
  where
    rSeq' :: RSeq a -> RAlt a -> [RSeq a]
    rSeq' (REmpty _) (RAlt seqs') = seqs'
    rSeq' (RSeq c1 (RAlt seqs')) alt' = [RSeq c1 (RAlt (rSeq' seq' alt')) | seq' <- seqs']

rAlt :: RAlt a -> RAlt a -> RAlt a
rAlt (RAlt seqs1) (RAlt seqs2) = RAlt $ seqs1 <> seqs2

rMany :: (Monoid a) => RAlt a -> RAlt a
rMany r =
  let RAlt seqs = rSeq r (rMany r)
   in RAlt (REmpty mempty : seqs)

match :: (Semigroup a) => Regex a -> String -> Maybe a
match r s = listToMaybe $ mapMaybe f $ matchAlt r s
  where
    f (a, "") = Just a
    f _ = Nothing

matchAlt :: (Semigroup a) => RAlt a -> String -> [(a, String)]
matchAlt (RAlt seqs) s = concat [matchSeq seq s | seq <- seqs]

matchSeq :: (Semigroup a) => RSeq a -> String -> [(a, String)]
matchSeq (REmpty a) s = [(a, s)]
matchSeq (RSeq char alt) s = do
  (a1, s1) <- matchChar char s
  (a2, s2) <- matchAlt alt s1
  pure (a1 <> a2, s2)

matchChar :: RChar a -> String -> [(a, String)]
matchChar (RChar rc a) (c : cs)
  | rc == c = [(a, cs)]
  | otherwise = []
matchChar _ [] = []

rChar :: Char -> Regex (Sum Int)
rChar c = RAlt [RSeq (RChar c 1) rEmpty]

regex0, regex1, regex2, regex3, regex4, regex5, regex6, regex7, regex8, regex9, regex10 :: Regex (Sum Int)
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

rChar' :: Char -> Regex String
rChar' c = RAlt [RSeq (RChar c [c]) rEmpty]

regex1', regex2', regex3', regex4', regex5', regex6', regex7', regex8', regex9', regex10' :: Regex String
regex1' = rEmpty -- //
regex2' = rChar' 'a' -- /a/
regex3' = rChar' 'a' `rSeq` rChar' 'b' -- /ab/
regex4' = rChar' 'a' `rAlt` rChar' 'b' -- /a|b/
regex5' = rEmpty `rAlt` rChar' 'a' -- /|a/
regex6' = rMany (rChar' 'a') -- /a*/
regex7' = (rChar' 'a' `rSeq` rChar' 'b') `rAlt` (rMany (rChar' 'c' `rSeq` rChar' 'd') `rSeq` rChar' 'e') -- /ab|(cd)*e/
regex8' = rMany (rChar' 'a') `rSeq` rChar' 'a' -- /a*a/
regex9' = ((rChar' 'a' `rAlt` rChar' 'b') `rSeq` rChar' 'c') `rAlt` (rChar' 'd' `rSeq` (rChar' 'e' `rAlt` rChar' 'f') `rSeq` rChar' 'g') -- (a|b)c|d(e|f)g
regex10' = rMany (rChar' 'a' `rAlt` rChar' 'b') `rSeq` rMany (rChar' 'a' `rAlt` rChar' 'c') -- (a|b)*(a|b|c)*

rChar'' :: Char -> Regex ()
rChar'' c = RAlt [RSeq (RChar c ()) rEmpty]

regex1'', regex2'', regex3'', regex4'', regex5'', regex6'', regex7'', regex8'', regex9'', regex10'' :: Regex ()
regex1'' = rEmpty -- //
regex2'' = rChar'' 'a' -- /a/
regex3'' = rChar'' 'a' `rSeq` rChar'' 'b' -- /ab/
regex4'' = rChar'' 'a' `rAlt` rChar'' 'b' -- /a|b/
regex5'' = rEmpty `rAlt` rChar'' 'a' -- /|a/
regex6'' = rMany (rChar'' 'a') -- /a*/
regex7'' = (rChar'' 'a' `rSeq` rChar'' 'b') `rAlt` (rMany (rChar'' 'c' `rSeq` rChar'' 'd') `rSeq` rChar'' 'e') -- /ab|(cd)*e/
regex8'' = rMany (rChar'' 'a') `rSeq` rChar'' 'a' -- /a*a/
regex9'' = ((rChar'' 'a' `rAlt` rChar'' 'b') `rSeq` rChar'' 'c') `rAlt` (rChar'' 'd' `rSeq` (rChar'' 'e' `rAlt` rChar'' 'f') `rSeq` rChar'' 'g') -- (a|b)c|d(e|f)g
regex10'' = rMany (rChar'' 'a' `rAlt` rChar'' 'b') `rSeq` rMany (rChar'' 'a' `rAlt` rChar'' 'c') -- (a|b)*(a|b|c)*
