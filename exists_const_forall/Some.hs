module Some where

import Data.Kind

type Some :: Type
data Some where
  MkSome :: a -> Some

someId :: Some -> Some
someId (MkSome a) = MkSome (id a)

someValue :: Some
someValue = MkSome (1 :: Int)

type SomeC :: (Type -> Constraint) -> Type
data SomeC c where
  MkSomeC :: (c a) => a -> SomeC c

someCShow :: SomeC Show -> String
someCShow (MkSomeC a) = show a

someCShowValue :: SomeC Show
someCShowValue = MkSomeC (1 :: Int)

type SomeF :: (k -> Type) -> Type
data SomeF f where
  MkSomeF :: f a -> SomeF f

lengthSomeF :: SomeF [] -> Int
lengthSomeF (MkSomeF l) = length l

someFValue :: SomeF []
someFValue = MkSomeF [1 :: Int]

type data V = V1 | V2

type SV :: V -> Type
data SV v where
  MkSV1 :: Int -> SV V1
  MkSV2 :: String -> SV V2

type SomeSV :: Type
data SomeSV where
  MkSomeSV :: SV v -> SomeSV

someSVShow :: SomeSV -> String
someSVShow (MkSomeSV sv) =
  case sv of
    MkSV1 n -> show n
    MkSV2 s -> s

someSV1, someSV2 :: SomeSV
someSV1 = MkSomeSV (MkSV1 1)
someSV2 = MkSomeSV (MkSV2 "x")

forward :: (SomeF f -> a) -> (forall x. f x -> a)
forward g = \fx -> g (MkSomeF fx)

backward :: (forall x. f x -> a) -> (SomeF f -> a)
backward h = \(MkSomeF fx) -> h fx

newtype Const a x = MkConst a

forward' :: (SomeF f -> a) -> (forall x. f x -> Const a x)
forward' g = \fx -> MkConst (g (MkSomeF fx))

backward' :: (forall x. f x -> Const a x) -> (SomeF f -> a)
backward' h = \(MkSomeF fx) -> let MkConst a = h fx in a

type f ~> g = forall x. f x -> g x

leftAdjunct :: (SomeF f -> a) -> (f ~> Const a)
leftAdjunct g = \fx -> MkConst (g (MkSomeF fx))

rightAdjunct :: (f  ~> Const a) -> (SomeF f -> a)
rightAdjunct h = \(MkSomeF fx) -> let MkConst a = h fx in a

fmap' :: Functor f => f a -> (a -> b) -> f b
fmap' = flip fmap

fmap'' :: Functor f => f x -> (x -> a) -> f a
fmap'' = fmap'

type SomeFA :: (Type -> Type) -> Type -> Type
data SomeFA f a where
  MkSomeFA :: f x -> (x -> a) -> SomeFA f a

fmap''' :: Functor f => SomeFA f a -> f a
fmap''' = \(MkSomeFA fa g) -> fmap'' fa g

fmap'''' :: Functor f => SomeFA f ~> f
fmap'''' = \(MkSomeFA fa g) -> fmap'' fa g

liftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2' = liftA2

liftA2'' :: Applicative f => (x -> y -> a) -> f x -> f y -> f a
liftA2'' = liftA2'

data SomeFA2 f a where
  MkSomeFA2 :: (x -> y -> a) -> f x -> f y -> SomeFA2 f a

liftA2''' :: Applicative f => SomeFA2 f a -> f a
liftA2''' (MkSomeFA2 g fx fy) = liftA2'' g fx fy
