{-# LANGUAGE NoImplicitPrelude #-}

import Data.Either (Either(..))
import Data.Function ((.), ($), const, flip, id)
import Data.List ((++), concat, repeat, zipWith)
import Data.Maybe (Maybe(..))
import Data.Monoid (Monoid(..))
import qualified Prelude as P
import System.IO (IO)

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor [] where
    fmap f (x:xs) = f x:fmap f xs
    fmap _ []     = []

instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap _ Nothing  = Nothing

instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap _ (Left y)  = Left y

instance Functor ((,) a) where
    fmap f (x, y) = (x, f y)

instance Functor ((->) a) where
    fmap = funmap

funmap :: (a -> b) -> (c -> a) -> (c -> b)
funmap f g = f . g

--instance Functor IO where
--    fmap f x = x >>= return . f


class Functor f => Pointed f where
    pure :: a -> f a

instance Pointed [] where
    pure = (:[])

instance Pointed Maybe where
    pure = Just

instance Pointed (Either a) where
    pure = Right

instance Pointed ((->) a) where
    pure = const

instance Monoid a => Pointed ((,) a) where
    pure x = (mempty, x)

--instance Pointed IO where
--    pure = return


class Pointed f => Applicative f where
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative [] where
    fs <*> xs = concat $ fmap (\f -> fmap f xs) fs

newtype ZipList a = ZipList [a]

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList (fmap f xs)

instance Pointed ZipList where
    pure x = ZipList (repeat x)

instance Applicative ZipList where
    ZipList fs <*> ZipList xs = ZipList $ zipWith ($) fs xs

instance Applicative Maybe where
    Just f <*> Just x = Just (f x)
    _      <*> _      = Nothing

instance Applicative (Either a) where
    Right f <*> Right x = Right (f x)
    _       <*> Left e  = Left e
    Left e  <*> _       = Left e

instance Monoid a => Applicative ((,) a) where
    (v, f) <*> (u, x) = (mappend v u, f x)

instance Applicative ((->) a) where
    (<*>) = fap

fap :: (a -> (b -> c)) -> (a -> b) -> (a -> c)
fap f g x = let h = f x
                y = g x
            in h y

--instance Applicative IO where
--    (<*>) = ap

(<$>) :: Applicative f => (a -> b) -> f a -> f b
f <$> x = pure f <*> x


class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b

instance Monad Maybe where
    Just x  >>= f = f x
    Nothing >>= _ = Nothing

instance Monad [] where
    (x:xs) >>= f = f x ++ xs >>= f
    []     >>= _ = []

instance Monad ((->) a) where
    (>>=) = fbind

fbind :: (a -> b) -> (b -> (a -> c)) -> a -> c
fbind f g x = let y = f x
                  h = g y
              in h x

join :: Monad m => m (m a) -> m a
join f = f >>= id

sequence :: Monad m => [m a] -> m [a]
sequence (x:xs) = x >>= \y -> sequence xs >>= \ys -> pure (y:ys)
sequence []     = pure []

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \x -> f x >>= g
