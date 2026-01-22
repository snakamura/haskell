module Profunctor where

import Control.Arrow ((&&&))
import Data.Profunctor
import Sample

type Lens s a = forall p. (Profunctor p, Strong p) => p a a -> p s s

lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens getter setter = dimap (id &&& getter) (uncurry setter) . second'

lens' :: forall s a. (s -> a) -> (s -> a -> s) -> Lens s a
lens' getter setter = \(p :: p a a) ->
  let p' :: p (x, a) (x, a)
      p' = second' p
      setter' :: (s, a) -> s
      setter' = uncurry setter
      p'' :: p (s, a) s
      p'' = rmap setter' p'
      getter' :: s -> (s, a)
      getter' s = (s, getter s)
      p''' :: p s s
      p''' = lmap getter' p''
   in p'''

view :: forall s a. Lens s a -> (s -> a)
view l =
  let paa :: Forget a a a
      paa = Forget id
      pss :: Forget a s s
      pss = l paa
   in runForget pss

set :: forall s a. Lens s a -> (a -> s -> s)
set l = \a ->
  let paa :: a -> a
      paa = const a
      pss :: s -> s
      pss = l paa
   in pss

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
