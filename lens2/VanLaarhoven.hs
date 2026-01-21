module VanLaarhoven where

import Data.Functor.Const
import Data.Functor.Identity
import Sample

type Lens s a = forall f. (Functor f) => (a -> f a) -> (s -> f s)

lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens getter setter = \a2fa -> \s ->
  let a = getter s
      fa = a2fa a
      a2s = setter s
   in fmap a2s fa

view :: Lens s a -> (s -> a)
view l = \s ->
  let a2fa = \a -> Const a
      Const a = l a2fa s
   in a

set :: Lens s a -> (a -> s -> s)
set l = \a s ->
  let a2fa = \_ -> Identity a
      Identity s' = l a2fa s
   in s'

name :: Lens Person Name
name = lens _name (\person name -> person {_name = name})

age :: Lens Person Int
age = lens _age (\person age -> person {_age = age})

first :: Lens Name String
first = lens _first (\name first -> name {_first = first})

last :: Lens Name String
last = lens _last (\name last -> name {_last = last})

compose :: Lens middle inner -> Lens outer middle -> Lens outer inner
compose m2i o2m = o2m . m2i

firstName :: Lens Person String
firstName = compose first name

scott :: String
scott = view firstName scottTiger

michealTiger :: Person
michealTiger = set firstName "Micheal" scottTiger
