module Regex15 where

import Control.Alternative.Free
import Control.Applicative (Alternative (..))
import Control.Monad (guard)
import Control.Monad.State
import Data.Functor
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

print :: Regex a -> IO ()
print r = runAlt printChar r *> putStrLn ""

printChar :: RChar a -> IO a
printChar (RChar rc a) = putChar rc $> a

list :: Regex a -> [a]
list = runAlt listChar

listChar :: RChar a -> [a]
listChar (RChar _ a) = [a]

rChar :: Char -> Regex String
rChar c = liftAlt (RChar c [c])

regex0, regex1, regex2, regex3, regex4, regex5, regex6, regex7, regex7_, regex8, regex9, regex10 :: Regex String
regex0 = empty
regex1 = pure "" -- //
regex2 = rChar 'a' -- /a/
regex3 = (<>) <$> rChar 'a' <*> rChar 'b' -- /ab/
regex4 = rChar 'a' <|> rChar 'b' -- /a|b/
regex5 = pure "" <|> rChar 'a' -- /|a/
regex6 = concat <$> many (rChar 'a') -- /a*/
regex7 = (<>) <$> rChar 'a' <*> rChar 'b' <|> (<>) <$> (concat <$> many ((<>) <$> rChar 'c' <*> rChar 'd')) <*> rChar 'e' -- /ab|(cd)*e/
regex7_ = const "" <$> rChar 'a' <* rChar 'b' <|> (concat <$> many ((<>) <$> rChar 'c' <*> rChar 'd')) <* rChar 'e' -- /ab|(cd)*e/
regex8 = (<>) <$> (concat <$> many (rChar 'a')) <*> rChar 'a' -- /a*a/
regex9 = (<>) <$> (rChar 'a' <|> rChar 'b') <*> rChar 'c' <|> (\a b c -> a <> b <> c) <$> rChar 'd' <*> (rChar 'e' <|> rChar 'f') <*> rChar 'g' -- (a|b)c|d(e|f)g
regex10 = (<>) <$> (concat <$> many (rChar 'a' <|> rChar 'b')) <*> (concat <$> many (rChar 'a' <|> rChar 'c')) -- (a|b)*(a|b|c)*
