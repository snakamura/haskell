module Regex16 where

import Control.Applicative (Alternative (..))
import Control.Monad (guard)
import Control.Monad.State (MonadState (..), StateT (..))
import Data.Function ((&))
import Data.Maybe (listToMaybe, mapMaybe)
import Prelude hiding (seq)

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

rAlt :: RAlt f a -> RAlt f a -> RAlt f a
rAlt (RAlt seqs1) (RAlt seqs2) = RAlt $ seqs1 <> seqs2

runAlt :: (Alternative g) => (forall x. f x -> g x) -> RAlt f a -> g a
runAlt m (RAlt seqs) = foldr (\seq alt -> runSeq m seq <|> alt) empty seqs

runSeq :: (Alternative g) => (forall x. f x -> g x) -> RSeq f a -> g a
runSeq _ (REmpty a) = pure a
runSeq m (RSeq fa alt) = (&) <$> m fa <*> runAlt m alt

data RChar a = RChar Char a deriving (Functor)

type Regex = RAlt RChar

rChar :: Char -> Regex String
rChar c = RAlt [RSeq (RChar c [c]) (rEmpty id)]

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

list :: Regex a -> [a]
list = runAlt listChar

listChar :: RChar a -> [a]
listChar (RChar _ a) = [a]

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
