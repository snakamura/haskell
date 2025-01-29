module Regex9 where

import Control.Applicative (Alternative (..))
import Data.Functor (void)
import Data.Maybe (listToMaybe, mapMaybe)
import Prelude hiding (seq)

data RChar a = RChar Char a deriving (Functor)

data RSeq a where
  REmpty :: a -> RSeq a
  RSeq :: RChar b -> RAlt (b -> a) -> RSeq a

deriving instance Functor RSeq

newtype RAlt a = RAlt [RSeq a] deriving (Functor)

instance Applicative RAlt where
  pure = rEmpty
  (<*>) = rSeq

instance Alternative RAlt where
  empty = RAlt []
  (<|>) = rAlt

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
rAlt (RAlt alt1) (RAlt alt2) = RAlt $ alt1 <> alt2

rMany :: RAlt a -> RAlt [a]
rMany r =
  let RAlt alt = rSeq ((:) <$> r) (rMany r)
   in RAlt (REmpty [] : alt)

match :: Regex a -> String -> Maybe a
match r s = listToMaybe $ mapMaybe f $ matchAlt r s
  where
    f (a, "") = Just a
    f _ = Nothing

matchAlt :: RAlt a -> String -> [(a, String)]
matchAlt (RAlt alt) s = concatMap (flip matchSeq s) alt

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

regex0, regex1, regex2, regex3, regex4, regex5, regex6, regex7, regex7_, regex8, regex9 :: Regex Int
regex0 = rNever
regex1 = pure 0 -- //
regex2 = rChar 'a' -- /a/
regex3 = (+) <$> rChar 'a' <*> rChar 'b' -- /ab/
regex4 = rChar 'a' <|> rChar 'b' -- /a|b/
regex5 = pure 0 <|> rChar 'a' -- /|a/
regex6 = sum <$> rMany (rChar 'a') -- /a*/
regex7 = (+) <$> rChar 'a' <*> rChar 'b' <|> (+) <$> (sum <$> rMany ((+) <$> rChar 'c' <*> rChar 'd')) <*> rChar 'e' -- /ab|(cd)*e/
regex7_ = const 0 <$> rChar 'a' <* rChar 'b' <|> (sum <$> rMany ((+) <$> rChar 'c' <*> rChar 'd')) <* rChar 'e' -- /ab|(cd)*e/
regex8 = (+) <$> (sum <$> rMany (rChar 'a')) <*> rChar 'a' -- /a*a/
regex9 = (+) <$> (rChar 'a' <|> rChar 'b') <*> rChar 'c' <|> (\a b c -> a + b + c) <$> rChar 'd' <*> (rChar 'e' <|> rChar 'f') <*> rChar 'g' -- (a|b)c|d(e|f)g

rChar' :: Char -> Regex String
rChar' c = RAlt [RSeq (RChar c [c]) (rEmpty id)]

regex1', regex2', regex3', regex4', regex5', regex6', regex7', regex7_', regex8', regex9' :: Regex String
regex1' = pure "" -- //
regex2' = rChar' 'a' -- /a/
regex3' = (<>) <$> rChar' 'a' <*> rChar' 'b' -- /ab/
regex4' = rChar' 'a' <|> rChar' 'b' -- /a|b/
regex5' = pure "" <|> rChar' 'a' -- /|a/
regex6' = mconcat <$> rMany (rChar' 'a') -- /a*/
regex7' = (<>) <$> rChar' 'a' <*> rChar' 'b' <|> (<>) <$> (mconcat <$> rMany ((<>) <$> rChar' 'c' <*> rChar' 'd')) <*> rChar' 'e' -- /ab|(cd)*e/
regex7_' = const "" <$> rChar' 'a' <* rChar' 'b' <|> (mconcat <$> rMany ((<>) <$> rChar' 'c' <*> rChar' 'd')) <* rChar' 'e' -- /ab|(cd)*e/
regex8' = (<>) <$> (mconcat <$> rMany (rChar' 'a')) <*> rChar' 'a' -- /a*a/
regex9' = (<>) <$> (rChar' 'a' <|> rChar' 'b') <*> rChar' 'c' <|> (\a b c -> a <> b <> c) <$> rChar' 'd' <*> (rChar' 'e' <|> rChar' 'f') <*> rChar' 'g' -- (a|b)c|d(e|f)g

rChar'' :: Char -> Regex ()
rChar'' c = RAlt [RSeq (RChar c ()) (rEmpty id)]

regex1'', regex2'', regex3'', regex4'', regex5'', regex6'', regex7'', regex7_'', regex8'', regex9'' :: Regex ()
regex1'' = pure () -- //
regex2'' = rChar'' 'a' -- /a/
regex3'' = void $ rChar'' 'a' *> rChar'' 'b' -- /ab/
regex4'' = rChar'' 'a' <|> rChar'' 'b' -- /a|b/
regex5'' = pure () <|> rChar'' 'a' -- /|a/
regex6'' = void $ rMany (rChar'' 'a') -- /a*/
regex7'' = (void $ rChar'' 'a' *> rChar'' 'b') <|> (void $ (void $ rMany (void $ rChar'' 'c' *> rChar'' 'd')) *> rChar'' 'e') -- /ab|(cd)*e/
regex7_'' = void $ rChar'' 'a' <* rChar'' 'b' <|> (void $ rMany (void $ rChar'' 'c' <* rChar'' 'd')) <* rChar'' 'e' -- /ab|(cd)*e/
regex8'' = void $ rMany (rChar'' 'a') *> rChar'' 'a' -- /a*a/
regex9'' = (rChar'' 'a' <|> rChar'' 'b') *> rChar'' 'c' <|> rChar'' 'd' *> (rChar'' 'e' <|> rChar'' 'f') *> rChar'' 'g' -- (a|b)c|d(e|f)g
