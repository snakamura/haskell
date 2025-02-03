module Regex8 where

import Data.Maybe (listToMaybe, mapMaybe)
import Prelude hiding (seq)

data RChar a = RChar Char a deriving (Functor)

data RSeq a where
  REmpty :: a -> RSeq a
  RSeq :: RChar b -> RAlt (b -> a) -> RSeq a

deriving instance Functor RSeq

newtype RAlt a = RAlt [RSeq a] deriving (Functor)

type Regex = RAlt

rNever :: RAlt a
rNever = RAlt []

rEmpty :: a -> RAlt a
rEmpty a = RAlt [REmpty a]

rSeq :: RAlt (a -> b) -> RAlt a -> RAlt b
rSeq (RAlt (seqs :: [RSeq (a -> b)])) (alt :: RAlt a) = RAlt $ concatMap (`rSeq'` alt) seqs
  where
    rSeq' :: RSeq (a -> b) -> RAlt a -> [RSeq b]
    rSeq' (REmpty (f :: a -> b)) (RAlt (seqs' :: [RSeq a])) = map (f <$>) seqs'
    rSeq' (RSeq (c :: RChar c) (alt1 :: RAlt (c -> a -> b))) (alt2 :: RAlt a) =
      [ RSeq
          (c :: RChar c)
          ((((flip <$> (alt1 :: RAlt (c -> a -> b))) :: RAlt (a -> c -> b)) `rSeq` (alt2 :: RAlt a)) :: RAlt (c -> b))
      ]

{-
rSeq' :: RSeq (a -> b) -> RSeq a -> RSeq b
rSeq' (REmpty f) seq = f <$> seq
rSeq' seq (REmpty a) = ($ a) <$> seq
rSeq' (RSeq c alt) seq = RSeq c ((flip <$> alt) `rSeq` RAlt [seq])
-}

rAlt :: RAlt a -> RAlt a -> RAlt a
rAlt (RAlt seqs1) (RAlt seqs2) = RAlt $ seqs1 <> seqs2

rMany :: RAlt a -> RAlt [a]
rMany r =
  let RAlt seqs = rSeq ((:) <$> r) (rMany r)
   in RAlt (REmpty [] : seqs)

match :: Regex a -> String -> Maybe a
match r s = listToMaybe $ mapMaybe f $ matchAlt r s
  where
    f (a, "") = Just a
    f _ = Nothing

matchAlt :: RAlt a -> String -> [(a, String)]
matchAlt (RAlt seqs) s = concat [matchSeq seq s | seq <- seqs]

matchSeq :: RSeq a -> String -> [(a, String)]
matchSeq (REmpty a) s = [(a, s)]
matchSeq (RSeq char alt) s = do
  (a1, s1) <- matchChar char s
  (a2, s2) <- matchAlt alt s1
  pure (a2 a1, s2)

matchChar :: RChar a -> String -> [(a, String)]
matchChar (RChar rc a) (c : cs)
  | rc == c = [(a, cs)]
  | otherwise = []
matchChar _ [] = []

rChar :: Char -> Regex Int
rChar c = RAlt [RSeq (RChar c 1) (rEmpty id)]

regex0, regex1, regex2, regex3, regex4, regex5, regex6, regex7, regex8, regex9, regex10 :: Regex Int
regex0 = rNever
regex1 = rEmpty 0 -- //
regex2 = rChar 'a' -- /a/
regex3 = rEmpty (+) `rSeq` rChar 'a' `rSeq` rChar 'b' -- /ab/
regex4 = rChar 'a' `rAlt` rChar 'b' -- /a|b/
regex5 = rEmpty 0 `rAlt` rChar 'a' -- /|a/
regex6 = rEmpty sum `rSeq` rMany (rChar 'a') -- /a*/
regex7 = (rEmpty (+) `rSeq` rChar 'a' `rSeq` rChar 'b') `rAlt` (rEmpty (+) `rSeq` (rEmpty sum `rSeq` rMany (rEmpty (+) `rSeq` rChar 'c' `rSeq` rChar 'd')) `rSeq` rChar 'e') -- /ab|(cd)*e/
regex8 = rEmpty (+) `rSeq` (rEmpty sum `rSeq` rMany (rChar 'a')) `rSeq` rChar 'a' -- /a*a/
regex9 = (rEmpty (+) `rSeq` (rChar 'a' `rAlt` rChar 'b') `rSeq` rChar 'c') `rAlt` (rEmpty (\a b c -> a + b + c) `rSeq` rChar 'd' `rSeq` (rChar 'e' `rAlt` rChar 'f') `rSeq` rChar 'g') -- (a|b)c|d(e|f)g
regex10 = rEmpty (+) `rSeq` (rEmpty sum `rSeq` rMany (rChar 'a' `rAlt` rChar 'b')) `rSeq` (rEmpty sum `rSeq` rMany (rChar 'a' `rAlt` rChar 'c')) -- (a|b)*(a|b|c)*

rChar' :: Char -> Regex String
rChar' c = RAlt [RSeq (RChar c [c]) (rEmpty id)]

regex1', regex2', regex3', regex4', regex5', regex6', regex7', regex8', regex9', regex10' :: Regex String
regex1' = rEmpty "" -- //
regex2' = rChar' 'a' -- /a/
regex3' = rEmpty (<>) `rSeq` rChar' 'a' `rSeq` rChar' 'b' -- /ab/
regex4' = rChar' 'a' `rAlt` rChar' 'b' -- /a|b/
regex5' = rEmpty "" `rAlt` rChar' 'a' -- /|a/
regex6' = rEmpty concat `rSeq` rMany (rChar' 'a') -- /a*/
regex7' = (rEmpty (<>) `rSeq` rChar' 'a' `rSeq` rChar' 'b') `rAlt` (rEmpty (<>) `rSeq` (rEmpty concat `rSeq` rMany (rEmpty (<>) `rSeq` rChar' 'c' `rSeq` rChar' 'd')) `rSeq` rChar' 'e') -- /ab|(cd)*e/
regex8' = rEmpty (<>) `rSeq` (rEmpty concat `rSeq` rMany (rChar' 'a')) `rSeq` rChar' 'a' -- /a*a/
regex9' = (rEmpty (<>) `rSeq` (rChar' 'a' `rAlt` rChar' 'b') `rSeq` rChar' 'c') `rAlt` (rEmpty (\a b c -> a <> b <> c) `rSeq` rChar' 'd' `rSeq` (rChar' 'e' `rAlt` rChar' 'f') `rSeq` rChar' 'g') -- (a|b)c|d(e|f)g
regex10' = rEmpty (<>) `rSeq` (rEmpty concat `rSeq` rMany (rChar' 'a' `rAlt` rChar' 'b')) `rSeq` (rEmpty concat `rSeq` rMany (rChar' 'a' `rAlt` rChar' 'c')) -- (a|b)*(a|b|c)*

rChar'' :: Char -> Regex ()
rChar'' c = RAlt [RSeq (RChar c ()) (rEmpty id)]

regex1'', regex2'', regex3'', regex4'', regex5'', regex6'', regex7'', regex8'', regex9'', regex10'' :: Regex ()
regex1'' = rEmpty () -- //
regex2'' = rChar'' 'a' -- /a/
regex3'' = rEmpty (const (const ())) `rSeq` rChar'' 'a' `rSeq` rChar'' 'b' -- /ab/
regex4'' = rChar'' 'a' `rAlt` rChar'' 'b' -- /a|b/
regex5'' = rEmpty () `rAlt` rChar'' 'a' -- /|a/
regex6'' = rEmpty (const ()) `rSeq` rMany (rChar'' 'a') -- /a*/
regex7'' = (rEmpty (const (const())) `rSeq` rChar'' 'a' `rSeq` rChar'' 'b') `rAlt` (rEmpty (const (const ())) `rSeq` (rEmpty (const ()) `rSeq` rMany (rEmpty (const (const ())) `rSeq` rChar'' 'c' `rSeq` rChar'' 'd')) `rSeq` rChar'' 'e') -- /ab|(cd)*e/
regex8'' = rEmpty (const (const ())) `rSeq` rMany (rChar'' 'a') `rSeq` rChar'' 'a' -- /a*a/
regex9'' = (rEmpty (const (const ())) `rSeq` (rChar'' 'a' `rAlt` rChar'' 'b') `rSeq` rChar'' 'c') `rAlt` (rEmpty (\_ _ _ -> ()) `rSeq` rChar'' 'd' `rSeq` (rChar'' 'e' `rAlt` rChar'' 'f') `rSeq` rChar'' 'g') -- (a|b)c|d(e|f)g
regex10'' = rEmpty (const (const ())) `rSeq` (rEmpty (const ()) `rSeq` rMany (rChar'' 'a' `rAlt` rChar'' 'b')) `rSeq` (rEmpty (const ()) `rSeq` rMany (rChar'' 'a' `rAlt` rChar'' 'c')) -- (a|b)*(a|b|c)*
