import Data.Functor.Compose

pairs0 :: Compose ((,) Int) ((,) Bool) Char
pairs0 = Compose (10, (True, 'a'))

pairs1 :: a -> Compose ((,) Int) ((,) Bool) a
pairs1 a = Compose (10, (True, a))

pairs2 :: p1 -> p2 -> a -> Compose ((,) p1) ((,) p2) a
pairs2 p1 p2 a = Compose (p1, (p2, a))

functions0 :: Compose ((->) Int) ((->) Bool) Char
functions0 = Compose (\_ -> \_ -> 'a')

functions1 :: a -> Compose ((->) Int) ((->) Bool) a
functions1 a = Compose (\_ -> \_ -> a)

functions2 :: (a -> b -> c) -> Compose ((->) a) ((->) b) c
functions2 f = Compose f

pairFunction0 :: a -> Compose ((->) Int) ((,) Bool) a
pairFunction0 a = Compose (\_ -> (True, a))

pairFunction1 :: (b -> a) -> p -> Compose ((->) b) ((,) p) a
pairFunction1 f p = Compose (\b -> (p, f b))

pairFunction2 :: (b -> a) -> b -> Compose ((->) b) ((,) b) a
pairFunction2 f p = Compose (\b -> (p, f b)) -- b -> (b, a)

functionPair0 :: a -> Compose ((,) Bool) ((->) Int) a
functionPair0 a = Compose (True, (\_ -> a))

functionPair1 :: p -> (b -> a) -> Compose ((,) p) ((->) b) a
functionPair1 p f = Compose (p, f)

functionPair2 :: b -> (b -> a) -> Compose ((,) b) ((->) b) a
functionPair2 p f = Compose (p, f) -- (b, b -> a)

unit' :: a -> (b -> (b, a))
unit' a = \b -> (b, a)

unit :: a -> Compose ((->) b) ((,) b) a
unit a = Compose (\b -> (b, a))

counit' :: (b, b -> a) -> a
counit' (b, f) = f b

counit :: Compose ((,) b) ((->) b) a -> a
counit (Compose (b, f)) = f b
