module Regex7 where

import Data.Maybe (listToMaybe, mapMaybe)
import Data.Monoid (Sum)
import Prelude hiding (seq)

data RChar a = RChar Char a

data RSeq a = REmpty a | RSeq (RChar a) (RAlt a)

newtype RAlt a = RAlt [RSeq a]

type Regex = RAlt

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

match :: (Monoid a) => RAlt a -> String -> Maybe a
match r s = listToMaybe $ mapMaybe f $ matchAlt r s
  where
    f ("", a) = Just a
    f _ = Nothing

matchAlt :: (Monoid a) => RAlt a -> String -> [(String, a)]
matchAlt (RAlt alt) s = concatMap (flip matchSeq s) alt

matchSeq :: (Monoid a) => RSeq a -> String -> [(String, a)]
matchSeq (REmpty a) s = [(s, a)]
matchSeq (RSeq char alt) s = do
  (s1, a1) <- matchChar char s
  (s2, a2) <- matchAlt alt s1
  pure (s2, a1 <> a2)

matchChar :: RChar a -> String -> [(String, a)]
matchChar (RChar rc a) (c : cs)
  | rc == c = [(cs, a)]
  | otherwise = []
matchChar _ [] = []

rChar :: Char -> Regex (Sum Int)
rChar c = RAlt [RSeq (RChar c 1) rEmpty]

regex1, regex2, regex3, regex4, regex5, regex6, regex7, regex8 :: Regex (Sum Int)
regex1 = rEmpty -- //
regex2 = rChar 'a' -- /a/
regex3 = rChar 'a' `rSeq` rChar 'b' -- /ab/
regex4 = rChar 'a' `rAlt` rChar 'b' -- /a|b/
regex5 = rEmpty `rAlt` rChar 'a' -- /|a/
regex6 = rMany (rChar 'a') -- /a*/
regex7 = (rChar 'a' `rSeq` rChar 'b') `rAlt` (rMany (rChar 'c' `rSeq` rChar 'd') `rSeq` rChar 'e') -- /ab|(cd)*e/
regex8 = rMany (rChar 'a') `rSeq` rChar 'a' -- /a*a/

rChar' :: Char -> Regex String
rChar' c = RAlt [RSeq (RChar c [c]) rEmpty]

regex1', regex2', regex3', regex4', regex5', regex6', regex7', regex8' :: Regex String
regex1' = rEmpty -- //
regex2' = rChar' 'a' -- /a/
regex3' = rChar' 'a' `rSeq` rChar' 'b' -- /ab/
regex4' = rChar' 'a' `rAlt` rChar' 'b' -- /a|b/
regex5' = rEmpty `rAlt` rChar' 'a' -- /|a/
regex6' = rMany (rChar' 'a') -- /a*/
regex7' = (rChar' 'a' `rSeq` rChar' 'b') `rAlt` (rMany (rChar' 'c' `rSeq` rChar' 'd') `rSeq` rChar' 'e') -- /ab|(cd)*e/
regex8' = rMany (rChar' 'a') `rSeq` rChar' 'a' -- /a*a/

rChar'' :: Char -> Regex ()
rChar'' c = RAlt [RSeq (RChar c ()) rEmpty]

regex1'', regex2'', regex3'', regex4'', regex5'', regex6'', regex7'', regex8'' :: Regex ()
regex1'' = rEmpty -- //
regex2'' = rChar'' 'a' -- /a/
regex3'' = rChar'' 'a' `rSeq` rChar'' 'b' -- /ab/
regex4'' = rChar'' 'a' `rAlt` rChar'' 'b' -- /a|b/
regex5'' = rEmpty `rAlt` rChar'' 'a' -- /|a/
regex6'' = rMany (rChar'' 'a') -- /a*/
regex7'' = (rChar'' 'a' `rSeq` rChar'' 'b') `rAlt` (rMany (rChar'' 'c' `rSeq` rChar'' 'd') `rSeq` rChar'' 'e') -- /ab|(cd)*e/
regex8'' = rMany (rChar'' 'a') `rSeq` rChar'' 'a' -- /a*a/
