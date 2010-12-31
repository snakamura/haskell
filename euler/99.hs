import Control.Monad
import Data.List
import Data.List.Split
import Data.Ord
import System.IO

value = do v <- values
           let l = map logValues v
           print $ snd $ maximumBy (comparing fst) $ zip l [1..]

logValues [n, e] = fromIntegral e*log (fromIntegral n)

values :: IO [[Int]]
values = liftM (map (map read . sepBy ",") . lines) $ readFile "99.txt"
