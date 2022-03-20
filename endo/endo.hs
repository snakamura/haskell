import Data.Monoid

x :: [Int]
x = ((1:) <> (2:)) [3]

y :: [Int]
y = appEndo (Endo (1:) <> Endo (2:)) [3]


class MySemigroup a where
    (<<>>) :: a -> a -> a

instance MySemigroup b => MySemigroup (a -> b) where
    (f <<>> g) a = f a <<>> g a

instance MySemigroup [a] where
    (<<>>) = (++)

x2 :: [Int]
x2 = ((1:) <<>> (2:)) [3]

instance MySemigroup (Endo a) where
    Endo f <<>> Endo g = Endo $ f . g

y2 :: [Int]
y2 = appEndo (Endo (1:) <<>> Endo (2:)) [3]


squash :: Monoid a => [a] -> (a -> b) -> b
squash xs f = f (mconcat xs)

slow :: [Int]
slow = squash (map (:[]) [1..10000000]) id

fast :: [Int]
fast = squash (map (Endo . (:)) [1..10000000]) (flip appEndo [])
