module Functor.NaturalTransformation where

import Data.Functor.Compose
import Data.List

type f ~> g = forall x. f x -> g x

-- Three functors from Hask to Hask
-- (,) Bool, Maybe, List and

pairToMaybe :: (,) Bool ~> Maybe
pairToMaybe (True, x) = Just x
pairToMaybe (False, _) = Nothing

maybeToList :: Maybe ~> List
maybeToList (Just x) = [x]
maybeToList Nothing = []

-- Vertical composition
--
--      -- (,) Bool ->
-- Hask ---- Maybe --> Hask
--      ---- List --->

(.|) ::
  (Functor f, Functor g, Functor h) =>
  (g ~> h) -> (f ~> g) -> (f ~> h)
gh .| fg = gh . fg

pairToList :: (,) Bool ~> List
pairToList = maybeToList .| pairToMaybe

-- Horizontal composition
--
--      -- (,) Bool ->      -- Maybe ->
-- Hask                Hask             Hask
--      ---- Maybe -->      -- List -->

(.-) ::
  (Functor f, Functor g, Functor j, Functor k) =>
  (j ~> k) -> (f ~> g) -> (Compose j f ~> Compose k g)
jk .- fg = \(Compose jfx) -> Compose (jk (fmap fg jfx))

type PairMaybe = Compose Maybe ((,) Bool)

type MaybeList = Compose List Maybe

pairMaybeToMaybeList :: PairMaybe ~> MaybeList
pairMaybeToMaybeList = maybeToList .- pairToMaybe
