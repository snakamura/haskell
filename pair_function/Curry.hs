module Curry where

f :: a -> b -> c
f = undefined

g :: (a, b) -> c
g = uncurry f

h :: a -> (b -> c)
h = f

leftAdjunct :: ((,) e a -> b) -> (a -> (->) e b)
leftAdjunct ea2b a e = ea2b (e, a)

rightAdjunct :: (a -> (->) e b) -> ((,) e a -> b)
rightAdjunct a2e2b (e, a) = a2e2b a e
