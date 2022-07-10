plus3 :: Int -> Int
plus3 = (+ 3)

double :: Int -> Int
double = (* 2)

composed' :: Int -> String
composed' x = show (double (plus3 x))

composed :: Int -> String
composed = show . double . plus3


plus3C :: Int -> (Int -> r) -> r
plus3C x f = f (x + 3)

doubleC :: Int -> (Int -> r) -> r
doubleC x f = f (x * 2)

showC :: Int -> (String -> r) -> r
showC x f = f (show x)

composedC' :: Int -> (String -> r) -> r
composedC' x f = plus3C x (flip doubleC (flip showC f))

(<.>) :: (b -> (c -> r) -> r) -> (a -> (b -> r) -> r) -> (a -> (c -> r) -> r)
(<.>) g f x h = f x (flip g h)

composedC :: Int -> (String -> r) -> r
composedC = showC <.> doubleC <.> plus3C


type Cont' r a = (a -> r) -> r

(<..>) :: (b -> Cont' r c) -> (a -> Cont' r b) -> (a -> Cont' r c)
(<..>) g f x h = f x (flip g h)
--(<..>) g f x = \h -> let c = f x
--                     in c $ \y -> let d = g y
--                                  in d h

unit' :: s -> Cont' a s
unit' x f = f x


newtype Cont r a = Cont ((a -> r) -> r)

(<...>) :: (b -> Cont r c) -> (a -> Cont r b) -> (a -> Cont r c)
(<...>) g f x = Cont $ \h -> let Cont c = f x
                             in c $ \y -> let Cont d = g y
                                          in d h

unit :: s -> Cont a s
unit x = Cont $ \f -> f x
