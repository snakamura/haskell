import Control.Monad
import Data.Array
import Data.Array.ST
import Data.List.Split

main = value

value = do list <- liftM (map (map read . sepBy ",") . lines) $ readFile "81.txt"
           let table = listArray ((0, 0), (79, 79)) $ concat list
           print $ fill table ! (79, 79)

fill :: Array (Int, Int) Int -> Array (Int, Int) Int
fill table = runSTArray $
  do let b@((lr, lc), (ur, uc)) = bounds table
     t <- newArray_ b
     forM_ [lr..ur] $ \r ->
       forM_ [lc..uc] $ \c ->
         do value <- v t r c
            writeArray t (r, c) value
     return t
    where
      v _ 0 0 = return $ table ! (0, 0)
      v t r c = do l <- read t r (c - 1)
                   u <- read t (r - 1) c
                   return $ min l u + table ! (r, c)
      read t r c | r < 0 || c < 0 = return maxBound
                 | otherwise      = readArray t (r, c)
