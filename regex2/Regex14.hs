module Regex14 where

import Control.Alternative.Free.Final
import Control.Applicative (Alternative (..))
import Control.Monad (guard)
import Control.Monad.State
import Data.Maybe (listToMaybe, mapMaybe)
import Prelude hiding (seq)

data RChar a = RChar Char a deriving (Functor)

type Regex = Alt RChar

match :: Regex a -> String -> Maybe a
match r s = listToMaybe $ mapMaybe f $ runStateT (runAlt matchChar r) s
  where
    f (a, "") = Just a
    f _ = Nothing

matchChar :: RChar a -> StateT String [] a
matchChar (RChar rc a) = do
  c : cs <- get
  guard (rc == c)
  put cs
  pure a

rChar :: Char -> Regex Int
rChar c = liftAlt (RChar c 1)

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
