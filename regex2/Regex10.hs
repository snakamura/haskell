module Regex10 where

import Control.Applicative (Alternative (..))
import Data.Maybe (listToMaybe, mapMaybe)
import Prelude hiding (seq)

data RChar a = RChar Char a deriving (Functor)

data RSeq f a where
  REmpty :: a -> RSeq f a
  RSeq :: f b -> RAlt f (b -> a) -> RSeq f a

deriving instance Functor (RSeq f)

newtype RAlt f a = RAlt [RSeq f a] deriving (Functor)

instance Applicative (RAlt f) where
  pure = rEmpty
  (<*>) = rSeq

instance Alternative (RAlt f) where
  empty = rNever
  (<|>) = rAlt

type Regex = RAlt RChar

rNever :: RAlt f a
rNever = RAlt []

rEmpty :: a -> RAlt f a
rEmpty a = RAlt [REmpty a]

rSeq :: RAlt f (a -> b) -> RAlt f a -> RAlt f b
rSeq (RAlt (seqs :: [RSeq f (a -> b)])) (alt :: RAlt f a) = RAlt $ concat [rSeq' seq alt | seq <- seqs]
  where
    rSeq' :: RSeq f (a -> b) -> RAlt f a -> [RSeq f b]
    rSeq' (REmpty (f :: a -> b)) (RAlt (seqs' :: [RSeq f a])) = map (f <$>) seqs'
    rSeq' (RSeq (c :: f c) (alt1 :: RAlt f (c -> a -> b))) (alt2 :: RAlt f a) =
      [ RSeq
          (c :: f c)
          ((((flip <$> (alt1 :: RAlt f (c -> a -> b))) :: RAlt f (a -> c -> b)) `rSeq` (alt2 :: RAlt f a)) :: RAlt f (c -> b))
      ]

{-
rSeq' :: RSeq (a -> b) -> RSeq a -> RSeq b
rSeq' (REmpty f) seq = f <$> seq
rSeq' seq (REmpty a) = ($ a) <$> seq
rSeq' (RSeq c alt) seq = RSeq c ((flip <$> alt) `rSeq` RAlt [seq])
-}

rAlt :: RAlt f a -> RAlt f a -> RAlt f a
rAlt (RAlt seqs1) (RAlt seqs2) = RAlt $ seqs1 <> seqs2

match :: Regex a -> String -> Maybe a
match r s = listToMaybe $ mapMaybe f $ matchAlt r s
  where
    f (a, "") = Just a
    f _ = Nothing

matchAlt :: RAlt RChar a -> String -> [(a, String)]
matchAlt (RAlt seqs) s = concat [matchSeq seq s | seq <- seqs]

matchSeq :: RSeq RChar a -> String -> [(a, String)]
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

regex0, regex1, regex2, regex3, regex4, regex5, regex6, regex7, regex7_, regex8, regex9, regex10 :: Regex Int
regex0 = empty
regex1 = pure 0 -- //
regex2 = rChar 'a' -- /a/
regex3 = (+) <$> rChar 'a' <*> rChar 'b' -- /ab/
regex4 = rChar 'a' <|> rChar 'b' -- /a|b/
regex5 = pure 0 <|> rChar 'a' -- /|a/
regex6 = sum <$> many (rChar 'a') -- /a*/
regex7 = (+) <$> rChar 'a' <*> rChar 'b' <|> (+) <$> (sum <$> many ((+) <$> rChar 'c' <*> rChar 'd')) <*> rChar 'e' -- /ab|(cd)*e/
regex7_ = const 0 <$> rChar 'a' <* rChar 'b' <|> (sum <$> many ((+) <$> rChar 'c' <*> rChar 'd')) <* rChar 'e' -- /ab|(cd)*e/
regex8 = (+) <$> (sum <$> many (rChar 'a')) <*> rChar 'a' -- /a*a/
regex9 = (+) <$> (rChar 'a' <|> rChar 'b') <*> rChar 'c' <|> (\a b c -> a + b + c) <$> rChar 'd' <*> (rChar 'e' <|> rChar 'f') <*> rChar 'g' -- (a|b)c|d(e|f)g
regex10 = (+) <$> (sum <$> many (rChar 'a' <|> rChar 'b')) <*> (sum <$> many (rChar 'a' <|> rChar 'c')) -- (a|b)*(a|b|c)*
