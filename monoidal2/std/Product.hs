module Product where

import Monoid

-- (Hask, (,), ()) is a monoidal category

instance MonoidalCategory (,) where
  type Unit (,) = ()

  assoc :: (a, (b, c)) -> ((a, b), c)
  assoc (a, (b, c)) = ((a, b), c)

  assocInv :: ((a, b), c) -> (a, (b, c))
  assocInv ((a, b), c) = (a, (b, c))

  left :: ((), a) -> a
  left ((), a) = a

  leftInv :: a -> ((), a)
  leftInv a = ((), a)

  right :: (a, ()) -> a
  right (a, ()) = a

  rightInv :: a -> (a, ())
  rightInv a = (a, ())

instance MonoidObject (,) Int where
  mu :: (Int, Int) -> Int
  mu (n, m) = n + m

  eta :: () -> Int
  eta () = 0

instance MonoidObject (,) [a] where
  mu :: ([a], [a]) -> [a]
  mu (a1, a2) = a1 ++ a2

  eta :: () -> [a]
  eta () = []

instance (MonoidObject (,) a) => MonoidObject (,) (Maybe a) where
  mu :: (Maybe a, Maybe a) -> Maybe a
  mu (Just a1, Just a2) = Just $ mu (a1, a2)
  mu (Just a1, _) = Just a1
  mu (_, Just a2) = Just a2
  mu _ = Nothing

  eta :: () -> Maybe a
  eta () = Nothing

instance MonoidObject (,) (a -> a) where
  mu :: (a -> a, a -> a) -> (a -> a)
  mu (f, g) = g . f

  eta :: () -> (a -> a)
  eta () = id

instance (MonoidObject (,) a, MonoidObject (,) b) => MonoidObject (,) (MonoidHomomorphism a b) where
  mu :: (MonoidHomomorphism a b, MonoidHomomorphism a b) -> MonoidHomomorphism a b
  mu (Hom f, Hom g) = Hom $ \a -> mu (f a, g a)

  eta :: () -> MonoidHomomorphism a b
  eta () = Hom (const (eta @(,) ()))

instance {-# OVERLAPPABLE #-} (MonoidObject (,) a) => Semigroup a where
  (<>) :: a -> a -> a
  (<>) a1 a2 = mu (a1, a2)

instance {-# OVERLAPPABLE #-} (MonoidObject (,) a) => Monoid a where
  mempty :: a
  mempty = eta @(,) ()

instance
  ( MonoidObject (,) m1,
    MonoidObject (,) m2,
    Eq m2
  ) =>
  MonoidHomomorphismLaws (,) m1 m2
  where
  preserveIdentity :: MonoidHomomorphism m1 m2 -> Bool
  preserveIdentity (Hom f) = f (eta @(,) ()) == eta @(,) ()

  preserveAppend :: MonoidHomomorphism m1 m2 -> (m1, m1) -> Bool
  preserveAppend (Hom f) (a, b) = f (mu (a, b)) == mu (f a, f b)

homLength :: MonoidHomomorphism [a] Int
homLength = Hom length

testPreserveIdentity, testPreserveAppend :: Bool
testPreserveIdentity = preserveIdentity @(,) homLength
testPreserveAppend = preserveAppend homLength (['A', 'B'], ['C', 'D', 'E'])
