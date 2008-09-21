import Data.List
import Data.Maybe
import Debug.Trace

value = reverse $ apply (1000000 `mod` num 9 - 1) next $ reverse "2013456789"
--value = reverse $ apply (1000000 - 1) next $ reverse "0123456789"

apply 0 _ v = v
apply n f v = apply (n - 1) f (f $! v)

next l = case findIndex (\[a, b] -> a > b) $ pairs l of
           Just i -> let (a, b) = splitAt (i + 1) l
                         v = head b
                         r = fromJust $ find (\n -> n > v) a
                     in reverse (sort (v:delete r a)) ++ r:tail b
           Nothing -> reverse l

pairs = filter ((== 2) . length) . map (take 2) . tails

num 1 = 1
num n = n*num (n - 1)


{-
value = perm "0123456789" !! (1000000 - 1)

perm [] = [[]]
perm l = [ a:b | a <- l, b <- perm (delete a l)]
-}
