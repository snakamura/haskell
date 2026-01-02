module Profunctor.NaturalTransformation where

import Data.Profunctor (Profunctor (..))
-- import Profunctor.Const
import Profunctor.Costar
import Profunctor.Pure
import Profunctor.Star

-- lmap f . alphaB . rmap f = rmap f . alphaA . lmap f

pureStar :: forall f a b. (Applicative f) => Pure a b -> Star f a b
pureStar (Pure a2b) = Star a2fb
  where
    a2fb :: a -> f b
    a2fb = pure . a2b

pureStarL,
  pureStarL',
  pureStarL'',
  pureStarL''',
  pureStarL'''',
  pureStarL''''' ::
    (Applicative f) => (a -> b) -> Pure b a -> Star f a b
pureStarL a2b = rmap a2b . pureStar . lmap a2b
pureStarL' a2b p = rmap a2b $ pureStar $ lmap a2b p
pureStarL'' a2b (Pure b2a) = rmap a2b $ pureStar $ Pure (b2a . a2b)
pureStarL''' a2b (Pure b2a) = rmap a2b $ Star (pure . b2a . a2b)
pureStarL'''' a2b (Pure b2a) = Star (fmap a2b . pure . b2a . a2b)
pureStarL''''' a2b (Pure b2a) = Star (pure . a2b . b2a . a2b) -- fmap a2b . pure = pure . a2b

pureStarR,
  pureStarR',
  pureStarR'',
  pureStarR''',
  pureStarR'''' ::
    (Applicative f) => (a -> b) -> Pure b a -> Star f a b
pureStarR a2b = lmap a2b . pureStar . rmap a2b
pureStarR' a2b p = lmap a2b $ pureStar $ rmap a2b p
pureStarR'' a2b (Pure b2a) = lmap a2b $ pureStar $ Pure (a2b . b2a)
pureStarR''' a2b (Pure b2a) = lmap a2b $ Star (pure . a2b . b2a)
pureStarR'''' a2b (Pure b2a) = Star (pure . a2b . b2a . a2b)

pureStarML,
  pureStarML',
  pureStarML'',
  pureStarML''' ::
    (Applicative f) => (a -> b) -> Pure b a -> Star f a b
pureStarML a2b = rmap a2b . lmap a2b . pureStar
pureStarML' a2b (Pure b2a) = rmap a2b $ lmap a2b $ Star (pure . b2a)
pureStarML'' a2b (Pure b2a) = rmap a2b $ Star (pure . b2a . a2b)
pureStarML''' a2b (Pure b2a) = Star (fmap a2b . pure . b2a . a2b)

pureStarMR,
  pureStarMR',
  pureStarMR'',
  pureStarMR''' ::
    (Applicative f) => (a -> b) -> Pure b a -> Star f a b
pureStarMR a2b = lmap a2b . rmap a2b . pureStar
pureStarMR' a2b (Pure b2a) = lmap a2b $ rmap a2b $ Star (pure . b2a)
pureStarMR'' a2b (Pure b2a) = lmap a2b $ Star (fmap a2b . pure . b2a)
pureStarMR''' a2b (Pure b2a) = Star (fmap a2b . pure . b2a . a2b)

hoistStar :: (forall x. f x -> g x) -> Star f a b -> Star g a b
hoistStar fx2gx (Star a2fa) = Star (fx2gx . a2fa)

hoistStarL,
  hoistStarL',
  hoistStarL'',
  hoistStarL''' ::
    (Functor f, Functor g) =>
    (a -> b) -> (forall x. f x -> g x) -> Star f b a -> Star g a b
hoistStarL a2b fx2gx = rmap a2b . hoistStar fx2gx . lmap a2b
hoistStarL' a2b fx2gx (Star b2fa) = rmap a2b $ hoistStar fx2gx $ Star (b2fa . a2b)
hoistStarL'' a2b fx2gx (Star b2fa) = rmap a2b $ Star (fx2gx . b2fa . a2b)
hoistStarL''' a2b fx2gx (Star b2fa) = Star (fmap a2b . fx2gx . b2fa . a2b)

hoistStarR,
  hoistStarR',
  hoistStarR'',
  hoistStarR''',
  hoistStarR'''' ::
    (Functor f, Functor g) =>
    (a -> b) -> (forall x. f x -> g x) -> Star f b a -> Star g a b
hoistStarR a2b fx2gx = lmap a2b . hoistStar fx2gx . rmap a2b
hoistStarR' a2b fx2gx (Star b2fa) = lmap a2b $ hoistStar fx2gx $ Star (fmap a2b . b2fa)
hoistStarR'' a2b fx2gx (Star b2fa) = lmap a2b $ Star (fx2gx . fmap a2b . b2fa)
hoistStarR''' a2b fx2gx (Star b2fa) = Star (fx2gx . fmap a2b . b2fa . a2b)
hoistStarR'''' a2b fx2gx (Star b2fa) = Star (fmap a2b . fx2gx . b2fa . a2b) -- fx2gx . fmap a2b = fmap a2b . fx2gx

hoistStarML, hoistStarML', hoistStarML'', hoistStarML''' ::
    (Functor f, Functor g) =>
    (a -> b) -> (forall x. f x -> g x) -> Star f b a -> Star g a b
hoistStarML a2b fx2gx = rmap a2b . lmap a2b . hoistStar fx2gx
hoistStarML' a2b fx2gx (Star b2fa) = rmap a2b $ lmap a2b $ Star (fx2gx . b2fa)
hoistStarML'' a2b fx2gx (Star b2fa) = rmap a2b $ Star (fx2gx . b2fa . a2b)
hoistStarML''' a2b fx2gx (Star b2fa) = Star (fmap a2b . fx2gx . b2fa . a2b)

hoistStarMR, hoistStarMR', hoistStarMR'', hoistStarMR''' ::
    (Functor f, Functor g) =>
    (a -> b) -> (forall x. f x -> g x) -> Star f b a -> Star g a b
hoistStarMR a2b fx2gx = lmap a2b . rmap a2b . hoistStar fx2gx
hoistStarMR' a2b fx2gx (Star b2fa) = lmap a2b $ rmap a2b $ Star (fx2gx . b2fa)
hoistStarMR'' a2b fx2gx (Star b2fa) = lmap a2b $ Star (fmap a2b . fx2gx . b2fa)
hoistStarMR''' a2b fx2gx (Star b2fa) = Star (fmap a2b . fx2gx . b2fa . a2b)

hoistCostar :: (forall x. g x -> f x) -> Costar f a b -> Costar g a b
hoistCostar gx2fx (Costar fa2a) = Costar (fa2a . gx2fx)

hoistCostarL,
  hoistCostarL',
  hoistCostarL'',
  hoistCostarL''' ::
    (Functor f, Functor g) =>
    (a -> b) -> (forall x. g x -> f x) -> Costar f b a -> Costar g a b
hoistCostarL a2b gx2fx = rmap a2b . hoistCostar gx2fx . lmap a2b
hoistCostarL' a2b gx2fx (Costar fb2a) = rmap a2b . hoistCostar gx2fx $ Costar (fb2a . fmap a2b)
hoistCostarL'' a2b gx2fx (Costar fb2a) = rmap a2b $ Costar (fb2a . fmap a2b . gx2fx)
hoistCostarL''' a2b gx2fx (Costar fb2a) = Costar (a2b . fb2a . fmap a2b . gx2fx)

hoistCostarR,
  hoistCostarR',
  hoistCostarR'',
  hoistCostarR''',
  hoistCostarR'''' ::
    (Functor f, Functor g) =>
    (a -> b) -> (forall x. g x -> f x) -> Costar f b a -> Costar g a b
hoistCostarR a2b gx2fx = lmap a2b . hoistCostar gx2fx . rmap a2b
hoistCostarR' a2b gx2fx (Costar fb2a) = lmap a2b $ hoistCostar gx2fx $ Costar (a2b . fb2a)
hoistCostarR'' a2b gx2fx (Costar fb2a) = lmap a2b $ Costar (a2b . fb2a . gx2fx)
hoistCostarR''' a2b gx2fx (Costar fb2a) = Costar (a2b . fb2a . gx2fx . fmap a2b)
hoistCostarR'''' a2b gx2fx (Costar fb2a) = Costar (a2b . fb2a . fmap a2b . gx2fx) -- gx2fx . fmap a2b = fmap a2b . gx2fx

hoistCostarML,
  hoistCostarML',
  hoistCostarML'',
  hoistCostarML''' ::
    (Functor f, Functor g) =>
    (a -> b) -> (forall x. g x -> f x) -> Costar f b a -> Costar g a b
hoistCostarML a2b fx2gx = rmap a2b . lmap a2b . hoistCostar fx2gx
hoistCostarML' a2b fx2gx (Costar fb2a) = rmap a2b $ lmap a2b $ Costar (fb2a . fx2gx)
hoistCostarML'' a2b fx2gx (Costar fb2a) = rmap a2b $ Costar (fb2a . fx2gx . fmap a2b)
hoistCostarML''' a2b fx2gx (Costar fb2a) = Costar (a2b . fb2a . fx2gx . fmap a2b)

hoistCostarMR,
  hoistCostarMR',
  hoistCostarMR'',
  hoistCostarMR''' ::
    (Functor f, Functor g) =>
    (a -> b) -> (forall x. g x -> f x) -> Costar f b a -> Costar g a b
hoistCostarMR a2b fx2gx = lmap a2b . rmap a2b . hoistCostar fx2gx
hoistCostarMR' a2b fx2gx (Costar fb2a) = lmap a2b $ rmap a2b $ Costar (fb2a . fx2gx)
hoistCostarMR'' a2b fx2gx (Costar fb2a) = lmap a2b $ Costar (a2b . fb2a . fx2gx)
hoistCostarMR''' a2b fx2gx (Costar fb2a) = Costar (a2b . fb2a . fx2gx . fmap a2b)

{-
constPure :: Const r a b -> Pure a b
constPure _ = Pure id

constPureL,
  constPureL',
  constPureL'',
  constPureL''',
  constPureL'''' ::
    (a -> b) -> Const r b a -> Pure a b
constPureL a2b = rmap a2b . constPure . lmap a2b
constPureL' a2b p = rmap a2b $ constPure $ lmap a2b p
constPureL'' a2b (Const r) = rmap a2b $ constPure (Const r)
constPureL''' a2b p = rmap a2b (Pure id)
constPureL'''' a2b p = Pure a2b

constPureR,
  constPureR',
  constPureR'',
  constPureR''',
  constPureR'''' ::
    (a -> b) -> Const r b a -> Pure a b
constPureR a2b = lmap a2b . constPure . rmap a2b
constPureR' a2b p = lmap a2b $ constPure $ rmap a2b p
constPureR'' a2b (Const r) = lmap a2b $ constPure $ Const r
constPureR''' a2b p = lmap a2b $ Pure id
constPureR'''' a2b p = Pure a2b
-}
