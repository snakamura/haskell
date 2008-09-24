{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State
import qualified Data.Map as Map

value = fst $ count [1, 2, 5, 10, 20, 50, 100, 200] 200 Map.empty

count _     0 mem = (1, mem)
count coins n mem = case Map.lookup (n, length coins) mem of
                      Just v -> (v, mem)
                      Nothing -> let usableCoins = takeWhile (<= n) coins
                                     f (sum, mem) coin = let (v, newMem) = count (takeWhile (<= coin) coins) (n - coin) mem
                                                         in (sum + v, newMem)
                                     vs = foldl f (0, mem) usableCoins
                                     v = fst vs
                                 in (v, Map.insert (n, length coins) v (snd vs))


valueS = evalState (countS [1, 2, 5, 10, 20, 50, 100, 200] 200) Map.empty

countS :: MonadState (Map.Map (Int, Int) Int) m => [Int] -> Int -> m Int
countS _     0 = return 1
countS coins n = do mem <- gets $ Map.lookup (n, length coins)
                    case mem of
                      Just v -> return v
                      Nothing -> do let usableCoins = takeWhile (<= n) coins
                                        f sum coin = liftM (sum +) $ countS (takeWhile (<= coin) coins) (n - coin)
                                    v <- foldM f 0 usableCoins
                                    modify $ Map.insert (n, length coins) v
                                    return v
