import Control.Monad
import Data.Char
import Data.List

value = head $ do
    x <- [1..]
    let v = f x
    guard (and $ map (\n -> f (n*x) == v) [2..6])
    return x

f = sort . show
