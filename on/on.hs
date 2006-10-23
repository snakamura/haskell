import Data.List

on :: (a -> a -> b) -> (c -> a) -> (c -> c -> b)
f `on` g = \x y -> g x `f` g y

x = [(1,2),(2,1),(1,3)]

y = sort x
z = sortBy (compare `on` snd) x
w = sortBy (\ (_, x) (_, y) -> compare x y) x
