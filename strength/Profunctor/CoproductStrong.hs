module Profunctor.CoproductStrong where

import Data.Either.Combinators
import Data.Profunctor (Profunctor (..))
import Functor.CoproductStrong
import Profunctor.Star

class (Profunctor p) => CoproductStrongProfunctor p where
  left :: p a b -> p (Either a c) (Either b c)
  right :: p a b -> p (Either c a) (Either c b)

instance CoproductStrongProfunctor (->) where
  left :: (a -> b) -> (Either a c -> Either b c)
  left a2b (Left a) = Left (a2b a)
  left _ (Right c) = Right c

  right :: (a -> b) -> (Either c a -> Either c b)
  right _ (Left c) = Left c
  right a2b (Right a) = Right (a2b a)

instance (CoproductStrongFunctor f) => CoproductStrongProfunctor (Star f) where
  left :: Star f a b -> Star f (Either a c) (Either b c)
  left (Star a2fb) = Star (\eac -> fmap swapEither (strength (fmap a2fb (swapEither eac))))

  right :: Star f a b -> Star f (Either c a) (Either c b)
  right (Star a2fb) = Star (\eca -> strength (fmap a2fb eca))

{-
instance (Applicative f) => CoproductStrongProfunctor (Star f) where
  left :: Star f a b -> Star f (Either a c) (Either b c)
  left (Star a2fb) =
    Star
      ( \cases
          (Left a) -> fmap Left (a2fb a)
          (Right c) -> pure (Right c)
      )

  right :: Star f a b -> Star f (Either c a) (Either c b)
  right (Star a2fb) =
    Star
      ( \cases
          (Left c) -> pure (Left c)
          (Right a) -> fmap Right (a2fb a)
      )
-}
