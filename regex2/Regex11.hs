module Regex11 where

import Control.Applicative (Alternative (..))
import Control.Monad (guard)
import Control.Monad.State
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
  empty = RAlt []
  (<|>) = rAlt

type Regex = RAlt RChar

rEmpty :: a -> RAlt f a
rEmpty a = RAlt [REmpty a]

rSeq :: RAlt f (a -> b) -> RAlt f a -> RAlt f b
rSeq (RAlt (seqs :: [RSeq f (a -> b)])) (alt :: RAlt f a) = RAlt $ concatMap (`rSeq'` alt) seqs
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
rAlt (RAlt alt1) (RAlt alt2) = RAlt $ alt1 <> alt2

match :: Regex a -> String -> Maybe a
match r s = listToMaybe $ mapMaybe f $ runStateT (matchAlt r) s
  where
    f (a, "") = Just a
    f _ = Nothing

matchAlt :: RAlt RChar a -> StateT String [] a
matchAlt (RAlt alt) = StateT $ \s -> concatMap (\seq -> runStateT (matchSeq seq) s) alt
--matchAlt (RAlt alt) = foldr (\seq a -> matchSeq seq <|> a) empty alt

matchSeq :: RSeq RChar a -> StateT String [] a
matchSeq (REmpty a) = pure a
matchSeq (RSeq char alt) = do
  a1 <- matchChar char
  a2 <- matchAlt alt
  pure (a2 a1)
--matchSeq (RSeq char alt) = (&) <$> matchChar char <*> matchAlt alt

matchChar :: RChar a -> StateT String [] a
matchChar (RChar rc a) = do
  c : cs <- get
  guard (rc == c)
  put cs
  pure a

rChar :: Char -> Regex Int
rChar c = RAlt [RSeq (RChar c 1) (rEmpty id)]

regex1, regex2, regex3, regex4, regex5, regex6, regex7, regex7_, regex8 :: Regex Int
regex1 = pure 0 -- //
regex2 = rChar 'a' -- /a/
regex3 = (+) <$> rChar 'a' <*> rChar 'b' -- /ab/
regex4 = rChar 'a' <|> rChar 'b' -- /a|b/
regex5 = pure 0 <|> rChar 'a' -- /|a/
regex6 = sum <$> many (rChar 'a') -- /a*/
regex7 = (+) <$> rChar 'a' <*> rChar 'b' <|> (+) <$> (sum <$> many ((+) <$> rChar 'c' <*> rChar 'd')) <*> rChar 'e' -- /ab|(cd)*e/
regex7_ = const 0 <$> rChar 'a' <* rChar 'b' <|> (sum <$> many ((+) <$> rChar 'c' <*> rChar 'd')) <* rChar 'e' -- /ab|(cd)*e/
regex8 = (+) <$> (sum <$> many (rChar 'a')) <*> rChar 'a' -- /a*a/
