import Data.Foldable
import Data.Functor.Const
import Data.Functor.Contravariant
import Data.Kind
import Data.Monoid
import Data.Profunctor

to0 :: (s -> a) -> ((a -> r) -> (s -> r))
to0 get = \ar -> \s ->
  let a = get s
      r = ar a
   in r

type Getter1 r s t a b = (a -> Const r b) -> (s -> Const r t)

to1 :: (s -> a) -> Getter1 r s t a b
to1 get = \afb -> \s ->
  let a = get s
      Const r = afb a
   in Const r

view1 :: Getter1 a s t a b -> s -> a
view1 getter s =
  let afb a = Const a
      sft = getter afb
      Const a = sft s
   in a

type Getter2 s t a b = forall f. (Functor f, Contravariant f) => (a -> f b) -> (s -> f t)

to2 :: (s -> a) -> Getter2 s t a b
to2 get = \afb -> \s ->
  let a = get s
      fb = afb a
   in () <$ fb $< ()

type Getter3 s a = forall f. (Functor f, Contravariant f) => (a -> f a) -> (s -> f s)

to3 :: (s -> a) -> Getter3 s a
to3 get = \afb -> \s ->
  let a = get s
      fb = afb a
   in () <$ fb $< ()

type Getter4 s a = forall p f. (Profunctor p, Functor f, Contravariant f) => p a (f a) -> p s (f s)

to4 :: (s -> a) -> Getter4 s a
to4 get = \pafa -> dimap get (contramap get) pafa

type Getting2 s a = (a -> Const a a) -> (s -> Const a s)

view2 :: Getting2 s a -> s -> a
view2 getter = getConst . getter Const

list1 :: Monoid a => (s -> [a]) -> Getter1 a s t a b
list1 getList = \afb -> \s ->
  let la = getList s
      fb = afb $ mconcat la
      Const a = fb
   in Const a

list1' :: forall s t a (b :: Type). Monoid a => (s -> [a]) -> Getter1 a s t a b
list1' getList = \afb -> \s ->
  let la = getList s
      fb = traverse_ afb la
      Const a = fb
   in Const a

type ListGetter2 s a = forall f. (Functor f, Contravariant f, Applicative f) => (a -> f a) -> (s -> f s)

list2 :: forall s t a (b :: Type). (s -> [a]) -> ListGetter2 s a
list2 getList = \afb -> \s ->
  let la = getList s
      fb = traverse_ afb la
   in fb $< ()

type Fold1 s t a b = (a -> Const a b) -> (s -> Const a t)

folding1 :: forall g s t a (b :: Type). (Foldable g, Monoid a) => (s -> g a) -> Fold1 s t a b
folding1 fold = \afb -> \s ->
  let ga = fold s
      fb = traverse_ afb ga
      Const a = fb
   in Const a

type Folding1 r s t a b = (a -> Const r b) -> (s -> Const r t)

views1 :: Folding1 r s t a b -> (a -> r) -> s -> r
views1 fold f s =
  let afb a = Const (f a)
      sft = fold afb
      Const t = sft s
   in t

type Fold2 s a = (a -> Const a a) -> (s -> Const a s)

folding2 :: forall g s t a (b :: Type). (Foldable g, Monoid a) => (s -> g a) -> Fold2 s a
folding2 fold = \afb -> \s ->
  let ga = fold s
      fb = traverse_ afb ga
      Const a = fb
   in Const a

type Folding2 r s a = (a -> Const r a) -> (s -> Const r s)

views2 :: Folding2 r s a -> (a -> r) -> s -> r
views2 fold f = getConst . fold (Const . f)

type Getting3 r s a = (a -> Const r a) -> (s -> Const r s)

view3 :: Getting3 a s a -> s -> a
view3 getter = getConst . getter Const

views3 :: Getting3 r s a -> (a -> r) -> s -> r
views3 fold f = getConst . fold (Const . f)

toListOf3 :: Getting3 [a] s a -> s -> [a]
toListOf3 fold s = views1 fold pure s

toListOf3' :: Getting3 (Endo [a]) s a -> s -> [a]
toListOf3' fold s = appEndo (views1 fold (Endo . (:)) s) []

type Fold3 s a = forall f. (Functor f, Contravariant f, Applicative f) => (a -> f a) -> (s -> f s)

folding3 :: Foldable g => (s -> g a) -> Fold3 s a
folding3 fold = \afb -> \s ->
  let ga = fold s
      fb = traverse_ afb ga
   in fb $< ()

type Getter s a = forall p f. (Profunctor p, Functor f, Contravariant f) => p a (f a) -> p s (f s)

type Fold s a = forall f. (Functor f, Contravariant f, Applicative f) => (a -> f a) -> (s -> f s)

type Getting r s a = (a -> Const r a) -> (s -> Const r s)

to :: (s -> a) -> Getter s a
to get = dimap get (contramap get)

folding :: Foldable g => (s -> g a) -> Fold s a
folding fold afb s = traverse_ afb (fold s) $< ()

view :: Getting a s a -> s -> a
view getter = getConst . getter Const

views :: Getting r s a -> (a -> r) -> s -> r
views fold f = getConst . fold (Const . f)

toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf fold s = appEndo (views fold (Endo . (:)) s) []

_1 :: Getter (a, b) a
_1 = to fst

both :: Fold (a, a) a
both = folding $ \(x, y) -> [x, y]

v1, v2 :: String
v1 = view _1 ("a", "b")
v2 = view both ("a", "b")

v3, v4 :: [String]
v3 = views _1 pure ("a", "b")
v4 = views both pure ("a", "b")

v5, v6 :: [String]
v5 = toListOf _1 ("a", "b")
v6 = toListOf both ("a", "b")
