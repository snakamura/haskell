import Control.Monad
import System.Environment

main = liftM (read . head) getArgs >>= print . column

column n = reverse $ takeWhile (/= ' ') $ map digit [0..]
 where
     digit d = (replicate (sum $ map (26^) [1..d]) ' ' ++ cycle (concatMap (replicate (26^d)) ['A'..'Z'])) !! n
