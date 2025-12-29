module Main where

main :: IO ()
main = pure ()

{-
F = List
G = Maybe

F = List
G = Maybe

m :: List Int -> Maybe Int

- Not natural transformation
m [] = Just 0
m (n:ns) = Just n

- Natural transformation taking the first element
m [] = Nothing
m (n:ns) = Just n

- Natural transformation taking the last element
m [] = Nothing
m (n:ns) = case ns of
              [] -> Just n
              _  -> m ns

l :: Lint Char -> Int

- Not natural transformation
l [] = 0
l (c:cs) = ord c

- Natural transformation returning the length
l [] = 0
l (c:cs) = 1 + l cs

x of τxc selects one of the natural transformations from the candidates.
c of τxc selects the type (Int for m and Char for l in this case).
-}

{-
m :: List Int -> Maybe Int
m [] = Nothing
m (n:_) = Just n

H c = List c -> Maybe c -- A functor from Hask to Set
-}
