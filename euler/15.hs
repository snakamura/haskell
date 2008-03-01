import Control.Monad
import Data.Array.IArray
import Data.Array.ST

value = (f 20) ! (20, 20)

{-
p _ 0 = 1
p 0 _ = 1
p x y = p (x - 1) y + p x (y - 1)
-}

candidates n = filter (\(x, y) -> x <= n && y <= n) $ concatMap g [0..n*2]
 where
     g n = [ (x, n - x) | x <- [0..n] ]

f n = runSTArray $ do
    a <- newArray_ ((0, 0), (n, n))
    mapM_ (fill a) $ candidates n
    return a
 where
     fill a i = value a i >>= writeArray a i
     value _ (0, 0) = return 0
     value _ (0, _) = return 1
     value _ (_, 0) = return 1
     value a (x, y) = liftM2 (+) (readArray a (x - 1, y)) (readArray a (x, y - 1))
