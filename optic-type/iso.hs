module Iso where

import Data.Functor.Const
import Data.Functor.Identity
import Data.Profunctor

type Iso1 s t a b = (a -> Const a b) -> (s -> Const a t)

iso1 :: (s -> a) -> (b -> t) -> Iso1 s t a b
iso1 to from = \afb -> \s ->
  let a = to s
      fb = afb a
      ft = fmap from fb
   in ft

type Iso2 s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

iso2 :: (s -> a) -> (b -> t) -> Iso2 s t a b
iso2 to from = \afb -> fmap from . afb . to

type Iso3 s t a b = forall f p. (Functor f, Profunctor p) => p a (f b) -> p s (f t)

iso3 :: (s -> a) -> (b -> t) -> Iso3 s t a b
iso3 to from = \pafb ->
  let sa = to
      fbft = fmap from
   in dimap sa fbft pafb

iso3' :: (s -> a) -> (b -> t) -> Iso3 s t a b
iso3' to from = dimap to (fmap from)

data Exchange3 a b s t = Exchange3 (s -> a) (b -> t)

instance Functor (Exchange3 a b s) where
   fmap f (Exchange3 sa bt) = Exchange3 sa (f . bt)

instance Profunctor (Exchange3 a b) where
   dimap f g (Exchange3 sa bt) = Exchange3 (sa . f) (g . bt)

{-
from3 :: (Exchange3 a b a (Identity b) -> Exchange3 s t s (Identity t)) -> b -> t
from3 iso = \b ->
   let Exchange3 sa bt = iso (Exchange3 id Identity)
    in (runIdentity . bt) b
-}

data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Functor (Exchange a b s) where
   fmap f (Exchange sa bt) = Exchange sa (f . bt)

instance Profunctor (Exchange a b) where
   dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)

type Iso s t a b = forall f p. (Functor f, Profunctor p) => p a (f b) -> p s (f t)

type AnIso s t a b = Exchange a b a (Identity b) -> Exchange s t s (Identity t)

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso to from = dimap to (fmap from)

{-
from :: AnIso s t a b -> b -> t
from iso = \b ->
   let Exchange sa bt = iso (Exchange id Identity)
    in (runIdentity . bt) b
-}
