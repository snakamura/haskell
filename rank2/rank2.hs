{-# OPTIONS_GHC -fglasgow-exts #-}

f :: (forall a. [a] -> Int) -> Int
f g = g [1,2,3] + g ['a','b','c']

{-
f' :: ([a] -> Int) -> Int
f' g = g [1,2,3] + g ['a','b','c']
-}

f2 :: (forall a. [a] -> Int) -> [a] -> [b] -> Int
f2 g x y = g x + g y

f3 :: ((forall a. [a] -> Int) -> [b] -> [c] -> Int) -> (forall a. [a] -> Int) -> [b] -> [c] -> Int
f3 g h x y = g h x y
