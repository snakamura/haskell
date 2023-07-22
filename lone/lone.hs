import Control.Applicative
import Data.Functor.Identity

class (Functor f) => Traversable' f where
  sequence' :: (Applicative g) => f (g a) -> g (f a)

instance Traversable' Identity where
  sequence' :: (Applicative g) => Identity (g a) -> g (Identity a)
  sequence' (Identity ga) = Identity <$> ga

instance Traversable' Maybe where
  sequence' :: (Applicative g) => Maybe (g a) -> g (Maybe a)
  sequence' Nothing = pure Nothing
  sequence' (Just ga) = Just <$> ga

instance Traversable' [] where
  sequence' :: (Applicative g) => [g a] -> g [a]
  sequence' [] = pure []
  sequence' (ga : gas) = (:) <$> ga <*> sequence' gas

instance Traversable' ((,) b) where
  sequence' :: (Applicative g) => (b, g a) -> g (b, a)
  sequence' (b, ga) = (b,) <$> ga

instance Traversable' (Either b) where
  sequence' :: (Applicative g) => Either b (g a) -> g (Either b a)
  sequence' (Left b) = pure (Left b)
  sequence' (Right ga) = Right <$> ga

instance Traversable' (Const b) where
  sequence' :: (Applicative g) => Const b (g a) -> g (Const b a)
  sequence' (Const b) = pure (Const b)

{-
instance Traversable' ((->) b) where
  sequence' :: Applicative g => (b -> g a) -> g (b -> a)
  sequence' = undefined
-}

maybe1, maybe2 :: Maybe [Int]
maybe1 = sequence' [pure 1, pure 2, empty]
maybe2 = sequence' [pure 1, pure 2]

io1, io2 :: IO [Int]
io1 = sequence' [pure 1, pure 2, empty]
io2 = sequence' [pure 1, pure 2]

io3, io4, io5 :: IO (Maybe Int)
io3 = sequence' (Just (pure 5))
io4 = sequence' (Just empty)
io5 = sequence' Nothing

class (Traversable' f) => Lone f where
  sequenceL :: (Functor g) => f (g a) -> g (f a)

instance Lone Identity where
  sequenceL :: (Functor g) => Identity (g a) -> g (Identity a)
  sequenceL (Identity ga) = Identity <$> ga

instance Lone ((,) b) where
  sequenceL :: (Functor g) => (b, g a) -> g (b, a)
  sequenceL (b, ga) = (b,) <$> ga
