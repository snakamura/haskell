module Store where

import Control.Comonad
import Sample

newtype Store a s = Store (a, a -> s)

instance Functor (Store a) where
  fmap :: (s -> t) -> (Store a s -> Store a t)
  fmap s2t (Store (a, a2s)) = Store (a, s2t . a2s)

instance Comonad (Store a) where
  extract :: Store a s -> s
  extract (Store (a, a2s)) = a2s a

  duplicate :: Store a s -> Store a (Store a s)
  duplicate (Store (a, a2s)) = Store (a, \a' -> Store (a', a2s))

type Lens s a = s -> Store a s

lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens getter setter = \s -> Store (getter s, setter s)

view :: Lens s a -> (s -> a)
view s2st = \s -> let Store (a, _) = s2st s in a

set :: Lens s a -> (a -> s -> s)
set s2st = \a s -> let Store (_, a2s) = s2st s in a2s a

name :: Lens Person Name
name = lens _name (\person name -> person {_name = name})

age :: Lens Person Int
age = lens _age (\person age -> person {_age = age})

first :: Lens Name String
first = lens _first (\name first -> name {_first = first})

last :: Lens Name String
last = lens _last (\name last -> name {_last = last})

compose :: Lens middle inner -> Lens outer middle -> Lens outer inner
compose m2i o2m =
  let getter = view m2i . view o2m
      setter s z = set o2m (set m2i z (view o2m s)) s
   in lens getter setter

compose' :: Lens middle inner -> Lens outer middle -> Lens outer inner
compose' m_i o_m = \o ->
  let Store (m, o2m) = o_m o
      Store (i, m2i) = m_i m
   in Store (i, \i -> o2m (m2i i))

compose'' :: Lens middle inner -> Lens outer middle -> Lens outer inner
compose'' m_i o_m = \o ->
  let Store (m, o2m) = o_m o
   in fmap o2m (m_i m)

firstName :: Lens Person String
firstName = compose first name

scott :: String
scott = view firstName scottTiger

michealTiger :: Person
michealTiger = set firstName "Micheal" scottTiger
