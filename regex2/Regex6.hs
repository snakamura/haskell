module Regex6 where

import Data.Maybe (listToMaybe, mapMaybe)
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

match :: Regex -> String -> Maybe Int
match r s = listToMaybe $ mapMaybe f $ matchAlt r s
  where
    f (n, "") = Just n
    f _ = Nothing

matchAlt :: RAlt -> String -> [(Int, String)]
matchAlt (RAlt alt) s = concatMap (flip matchSeq s) alt

matchSeq :: RSeq -> String -> [(Int, String)]
matchSeq REmpty s = [(0, s)]
matchSeq (RSeq char alt) s = do
  (n1, s1) <- matchChar char s
  (n2, s2) <- matchAlt alt s1
  pure (n1 + n2, s2)

matchChar :: RChar -> String -> [(Int, String)]
matchChar (RChar rc) (c : cs)
  | rc == c = [(1, cs)]
  | otherwise = []
matchChar _ [] = []
