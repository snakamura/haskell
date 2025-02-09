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
rSeq (RAlt (seqs :: [RSeq (a -> b)])) (alt :: RAlt a) = RAlt $ concat [rSeq' seq alt | seq <- seqs]
  where
    rSeq' :: RSeq (a -> b) -> RAlt a -> [RSeq b]
    rSeq' (REmpty (f :: a -> b)) (RAlt (seqs' :: [RSeq a])) = map (f <$>) seqs'
    rSeq' (RSeq (c :: RChar c) (alt1 :: RAlt (c -> a -> b))) (alt2 :: RAlt a) =
      [ RSeq
          (c :: RChar c)
          ((((flip <$> (alt1 :: RAlt (c -> a -> b))) :: RAlt (a -> c -> b)) `rSeq` (alt2 :: RAlt a)) :: RAlt (c -> b))
      ]

infixl 4 `rSeq`

{-
rSeq' :: RSeq (a -> b) -> RSeq a -> RSeq b
rSeq' (REmpty f) seq = f <$> seq
rSeq' seq (REmpty a) = ($ a) <$> seq
rSeq' (RSeq c alt) seq = RSeq c ((flip <$> alt) `rSeq` RAlt [seq])
-}

rAlt :: RAlt a -> RAlt a -> RAlt a
rAlt (RAlt seqs1) (RAlt seqs2) = RAlt $ seqs1 <> seqs2

infixl 3 `rAlt`

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

rChar :: Char -> RAlt Int
rChar c = RAlt [RSeq (RChar c 1) (rEmpty id)]

regex0, regex1, regex2, regex3, regex4, regex5, regex6, regex7, regex8, regex9, regex10 :: Regex Int
regex0 = rNever
regex1 = rEmpty 0 -- //
regex2 = rChar 'a' -- /a/
regex3 = (+) <$> rChar 'a' `rSeq` rChar 'b' -- /ab/
regex4 = rChar 'a' `rAlt` rChar 'b' -- /a|b/
regex5 = rEmpty 0 `rAlt` rChar 'a' -- /|a/
regex6 = sum <$> rMany (rChar 'a') -- /a*/
regex7 = (+) <$> rChar 'a' `rSeq` rChar 'b' `rAlt` (+) <$> (sum <$> rMany ((+) <$> rChar 'c' `rSeq` rChar 'd')) `rSeq` rChar 'e' -- /ab|(cd)*e/
regex8 = (+) <$> sum <$> rMany (rChar 'a') `rSeq` rChar 'a' -- /a*a/
regex9 = (+) <$> (rChar 'a' `rAlt` rChar 'b') `rSeq` rChar 'c' `rAlt` (\a b c -> a + b + c) <$> rChar 'd' `rSeq` (rChar 'e' `rAlt` rChar 'f') `rSeq` rChar 'g' -- (a|b)c|d(e|f)g
regex10 = (+) <$> sum <$> rMany (rChar 'a' `rAlt` rChar 'b') `rSeq` (sum <$> rMany (rChar 'a' `rAlt` rChar 'c')) -- (a|b)*(a|b|c)*

rChar' :: Char -> RAlt String
rChar' c = RAlt [RSeq (RChar c [c]) (rEmpty id)]

regex1', regex2', regex3', regex4', regex5', regex6', regex7', regex8', regex9', regex10' :: Regex String
regex1' = rEmpty "" -- //
regex2' = rChar' 'a' -- /a/
regex3' = (<>) <$> rChar' 'a' `rSeq` rChar' 'b' -- /ab/
regex4' = rChar' 'a' `rAlt` rChar' 'b' -- /a|b/
regex5' = rEmpty "" `rAlt` rChar' 'a' -- /|a/
regex6' = concat <$> rMany (rChar' 'a') -- /a*/
regex7' = (<>) <$> rChar' 'a' `rSeq` rChar' 'b' `rAlt` (<>) <$> (concat <$> rMany ((<>) <$> rChar' 'c' `rSeq` rChar' 'd')) `rSeq` rChar' 'e' -- /ab|(cd)*e/
regex8' = (<>) <$> concat <$> rMany (rChar' 'a') `rSeq` rChar' 'a' -- /a*a/
regex9' = (<>) <$> (rChar' 'a' `rAlt` rChar' 'b') `rSeq` rChar' 'c' `rAlt` (\a b c -> a <> b <> c) <$> rChar' 'd' `rSeq` (rChar' 'e' `rAlt` rChar' 'f') `rSeq` rChar' 'g' -- (a|b)c|d(e|f)g
regex10' = (<>) <$> concat <$> rMany (rChar' 'a' `rAlt` rChar' 'b') `rSeq` (concat <$> rMany (rChar' 'a' `rAlt` rChar' 'c')) -- (a|b)*(a|b|c)*

rChar'' :: Char -> RAlt ()
rChar'' c = RAlt [RSeq (RChar c ()) (rEmpty id)]

regex1'', regex2'', regex3'', regex4'', regex5'', regex6'', regex7'', regex8'', regex9'', regex10'' :: Regex ()
regex1'' = rEmpty () -- //
regex2'' = rChar'' 'a' -- /a/
regex3'' = const (const ()) <$> rChar'' 'a' `rSeq` rChar'' 'b' -- /ab/
regex4'' = rChar'' 'a' `rAlt` rChar'' 'b' -- /a|b/
regex5'' = rEmpty () `rAlt` rChar'' 'a' -- /|a/
regex6'' = const () <$> rMany (rChar'' 'a') -- /a*/
regex7'' = const (const()) <$> rChar'' 'a' `rSeq` rChar'' 'b' `rAlt` const (const ()) <$> (const () <$> rMany (const (const ()) <$> rChar'' 'c' `rSeq` rChar'' 'd')) `rSeq` rChar'' 'e' -- /ab|(cd)*e/
regex8'' = const (const ()) <$> rMany (rChar'' 'a') `rSeq` rChar'' 'a' -- /a*a/
regex9'' = const (const ()) <$> (rChar'' 'a' `rAlt` rChar'' 'b') `rSeq` rChar'' 'c' `rAlt` (\_ _ _ -> ()) <$> rChar'' 'd' `rSeq` (rChar'' 'e' `rAlt` rChar'' 'f') `rSeq` rChar'' 'g' -- (a|b)c|d(e|f)g
regex10'' = const (const ()) <$> const () <$> rMany (rChar'' 'a' `rAlt` rChar'' 'b') `rSeq` (const () <$> rMany (rChar'' 'a' `rAlt` rChar'' 'c')) -- (a|b)*(a|b|c)*
