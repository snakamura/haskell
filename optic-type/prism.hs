module Prism where

import Data.Functor.Identity
import Data.Profunctor
import Data.Tagged

type Prism1 s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)

prism1 :: (s -> Either t a) -> (b -> t) -> Prism1 s t a b
prism1 sa bt = \afb -> \s ->
  case sa s of
    Left t -> pure t
    Right a ->
      let fb = afb a
       in fmap bt fb

prism1' :: (s -> Either t a) -> (b -> t) -> Prism1 s t a b
prism1' sa bt = \afb -> \s ->
  either pure (fmap bt . afb) (sa s)

prism1'' :: (s -> Either t a) -> (b -> t) -> Prism1 s t a b
prism1'' sa bt = \afb ->
  either pure (fmap bt . afb) . sa

prism1''' :: (s -> Either t a) -> (b -> t) -> Prism1 s t a b
prism1''' sa bt = \afb ->
  (either pure (fmap bt) . fmap afb) . sa

prism1'''' :: (s -> Either t a) -> (b -> t) -> Prism1 s t a b
prism1'''' sa bt = \afb ->
  either pure (fmap bt) . fmap afb . sa

prism1''''' :: (s -> Either t a) -> (b -> t) -> Prism1 s t a b
prism1''''' sa bt = \afb ->
  dimap sa (either pure (fmap bt)) (fmap afb)

prism1'''''' :: (s -> Either t a) -> (b -> t) -> Prism1 s t a b
prism1'''''' sa bt = dimap sa (either pure (fmap bt)) . fmap

type Prism2 s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)

-- type Prism2 s t a b = forall p f. (Profunctor p, Applicative f) => p a (f b) -> p s (f t)

prism2 :: (s -> Either t a) -> (b -> t) -> Prism2 s t a b
prism2 sa bt = dimap sa (either pure (fmap bt)) . fmap

type Prism3 s t a b = forall p f. (Profunctor p, Choice p, Applicative f) => p a (f b) -> p s (f t)
type APrism3 s t a b = Tagged a (Identity b) -> Tagged s (Identity t)

prism3 :: (s -> Either t a) -> (b -> t) -> Prism3 s t a b
prism3 sa bt = dimap sa (either pure (fmap bt)) . right'

review3 :: APrism s t a b -> b -> t
-- review3 prism = fmap (runIdentity . unTagged . prism . Tagged . Identity) ask
-- review3 prism = runIdentity . unTagged . prism . Tagged . Identity
review3 prism = \b ->
  let pafb = Tagged $ Identity b
      psft = prism pafb
      t = runIdentity $ unTagged psft
   in t

type Prism s t a b = forall p f. (Profunctor p, Choice p, Applicative f) => p a (f b) -> p s (f t)

type APrism s t a b = Tagged a (Identity b) -> Tagged s (Identity t)

prism :: (s -> Either t a) -> (b -> t) -> Prism s t a b
prism sa bt = dimap sa (either pure (fmap bt)) . right'

review :: APrism s t a b -> b -> t
review prism = runIdentity . unTagged . prism . Tagged . Identity
