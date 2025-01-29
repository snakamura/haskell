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
rSeq (RAlt alt1) alt2 = RAlt $ concat [rSeq' a1 alt2 | a1 <- alt1]
  where
    rSeq' :: RSeq a -> RAlt a -> [RSeq a]
    rSeq' (REmpty _) (RAlt alt) = alt
    rSeq' (RSeq c1 (RAlt alt1')) alt2' = [RSeq c1 (RAlt (rSeq' a1 alt2')) | a1 <- alt1']

rAlt :: RAlt a -> RAlt a -> RAlt a
rAlt (RAlt alt1) (RAlt alt2) = RAlt $ alt1 <> alt2

rMany :: (Monoid a) => RAlt a -> RAlt a
rMany r =
  let RAlt alt = rSeq r (rMany r)
   in RAlt (REmpty mempty : alt)

match :: (Monoid a) => Regex a -> String -> Maybe a
match r s = listToMaybe $ mapMaybe f $ matchAlt r s
  where
    f (a, "") = Just a
    f _ = Nothing

matchAlt :: (Monoid a) => RAlt a -> String -> [(a, String)]
matchAlt (RAlt alt) s = concatMap (flip matchSeq s) alt

matchSeq :: (Monoid a) => RSeq a -> String -> [(a, String)]
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

regex0, regex1, regex2, regex3, regex4, regex5, regex6, regex7, regex8, regex9 :: Regex (Sum Int)
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

rChar' :: Char -> Regex String
rChar' c = RAlt [RSeq (RChar c [c]) rEmpty]

regex1', regex2', regex3', regex4', regex5', regex6', regex7', regex8', regex9' :: Regex String
regex1' = rEmpty -- //
regex2' = rChar' 'a' -- /a/
regex3' = rChar' 'a' `rSeq` rChar' 'b' -- /ab/
regex4' = rChar' 'a' `rAlt` rChar' 'b' -- /a|b/
regex5' = rEmpty `rAlt` rChar' 'a' -- /|a/
regex6' = rMany (rChar' 'a') -- /a*/
regex7' = (rChar' 'a' `rSeq` rChar' 'b') `rAlt` (rMany (rChar' 'c' `rSeq` rChar' 'd') `rSeq` rChar' 'e') -- /ab|(cd)*e/
regex8' = rMany (rChar' 'a') `rSeq` rChar' 'a' -- /a*a/
regex9' = ((rChar' 'a' `rAlt` rChar' 'b') `rSeq` rChar' 'c') `rAlt` (rChar' 'd' `rSeq` (rChar' 'e' `rAlt` rChar' 'f') `rSeq` rChar' 'g') -- (a|b)c|d(e|f)g

rChar'' :: Char -> Regex ()
rChar'' c = RAlt [RSeq (RChar c ()) rEmpty]

regex1'', regex2'', regex3'', regex4'', regex5'', regex6'', regex7'', regex8'', regex9'' :: Regex ()
regex1'' = rEmpty -- //
regex2'' = rChar'' 'a' -- /a/
regex3'' = rChar'' 'a' `rSeq` rChar'' 'b' -- /ab/
regex4'' = rChar'' 'a' `rAlt` rChar'' 'b' -- /a|b/
regex5'' = rEmpty `rAlt` rChar'' 'a' -- /|a/
regex6'' = rMany (rChar'' 'a') -- /a*/
regex7'' = (rChar'' 'a' `rSeq` rChar'' 'b') `rAlt` (rMany (rChar'' 'c' `rSeq` rChar'' 'd') `rSeq` rChar'' 'e') -- /ab|(cd)*e/
regex8'' = rMany (rChar'' 'a') `rSeq` rChar'' 'a' -- /a*a/
regex9'' = ((rChar'' 'a' `rAlt` rChar'' 'b') `rSeq` rChar'' 'c') `rAlt` (rChar'' 'd' `rSeq` (rChar'' 'e' `rAlt` rChar'' 'f') `rSeq` rChar'' 'g') -- (a|b)c|d(e|f)g
