import Control.Monad
import System.Environment

main = liftM head getArgs >>= print . permutation

permutation l  = p $ length l
 where
     p 0 = [[]]
     p n = [ x:y | x <- l, y <- filter (notElem x) $ p $ n - 1 ]
