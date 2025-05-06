module Curry where

f :: a -> b -> c
f = undefined

g :: (a, b) -> c
g = uncurry f

h :: a -> (b -> c)
h = f
