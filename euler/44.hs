import Control.Monad
import Control.Monad.List
import Control.Monad.State
import qualified Data.Map as Map

value = head values

values = concatMap f [2..]
  where
    f n = do m <- [n - 1, n - 2..1]
             let j = pentagonal m
                 k = pentagonal n
             guard $ isPentagonal (j + k)
             guard $ isPentagonal (k - j)
             return $ k - j

pentagonals = map pentagonal [1..]

pentagonal n = n*(3*n - 1) `div` 2

isPentagonal n = isPentagonal' pentagonals
  where
    isPentagonal' (x:xs) | x > n     = False
                         | x == n    = True
                         | otherwise = isPentagonal' xs


valueS = head valuesS

valuesS = evalState (liftM concat $ mapM f [2..]) Map.empty
  where
    f n = liftM concat $ mapM (f' n) [n - 1, n - 2..1]
    f' n m = do let j = pentagonal m
                    k = pentagonal n
                sum <- isPentagonalS (j + k)
                diff <- isPentagonalS (k - j)
                return $ if sum && diff then [k - j] else []

isPentagonalS n =
  do t <- get
     case Map.lookup n t of
       Just v -> return v
       Nothing -> do let v = isPentagonal n
                     put $ Map.insert n v t
                     return v


{-
valuesSS = concat $ evalStateT (mapM f [2..]) Map.empty

f n = do m <- lift [n - 1, n - 2..1]
         let j = pentagonal m
             k = pentagonal n
         sum <- isPentagonalS (j + k)             
         diff <- isPentagonalS (k - j)
         lift $ guard $ sum && diff
         return $ k - j
-}
