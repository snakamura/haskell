module Functor.NaturalTransformation where

import Data.Functor.Compose
import Data.List

type f ~> g = forall x. f x -> g x

{-
maybeToList :: Maybe ~> []
maybeToList (Just x) = [x]
maybeToList Nothing = []
-}

maybeToListL,
  maybeToListL',
  maybeToListL'',
  maybeToListL''' ::
    (a -> b) -> Maybe a -> [b]
maybeToListL a2b = fmap a2b . maybeToList
maybeToListL' a2b Nothing = fmap a2b [] $ maybeToList Nothing
maybeToListL' a2b (Just a) = fmap a2b [a] $ maybeToList (Just a)
maybeToListL'' a2b Nothing = fmap a2b []
maybeToListL'' a2b (Just a) = fmap a2b [a]
maybeToListL''' a2b Nothing = []
maybeToListL''' a2b (Just a) = [a2b a]

maybeToListR,
  maybeToListR',
  maybeToListR'',
  maybeToListR''' ::
    (a -> b) -> Maybe a -> [b]
maybeToListR a2b = maybeToList . fmap a2b
maybeToListR' a2b Nothing = maybeToList $ fmap a2b Nothing
maybeToListR' a2b (Just a) = maybeToList $ fmap a2b (Just a)
maybeToListR'' a2b Nothing = maybeToList Nothing
maybeToListR'' a2b (Just a) = maybeToList (Just (a2b a))
maybeToListR''' a2b Nothing = []
maybeToListR''' a2b (Just a) = [a2b a]

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
