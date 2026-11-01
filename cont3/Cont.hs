module Cont where

newtype Cont r a = Cont ((a -> r) -> r)

instance Functor (Cont r) where
  fmap :: (a -> b) -> (Cont r a -> Cont r b)
  fmap a2b (Cont ar2r) = Cont $ \b2r -> ar2r (\a -> b2r (a2b a))

instance Applicative (Cont r) where
  pure :: a -> Cont r a
  pure a = Cont $ \a2r -> a2r a

  (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
  (<*>) (Cont abr2r) (Cont ar2r) = Cont $ \b2r -> abr2r $ \a2b -> ar2r $ \a -> b2r (a2b a)

instance Monad (Cont r) where
  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  (>>=) (Cont ar2r) a2crb = Cont $ \b2r -> ar2r $ \a -> let (Cont br2r) = a2crb a in br2r b2r
