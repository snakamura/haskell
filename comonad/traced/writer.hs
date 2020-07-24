import Control.Monad
    ( (>>=)
    , (>=>)
    )
import Control.Monad.Writer
    ( Writer
    , tell
    , runWriter
    )
import Data.Functor (($>))

add :: Int -> Writer String Int
add v = tell ("Adding 10 to " ++ show v ++ "\n") $> v + 10

mul :: Int -> Writer String Int
mul v = tell ("Multiplying " ++ show v ++ " by 2\n") $> v * 2

result, result' :: (Int, String)
result = runWriter (add 5 >>= mul)
result' = runWriter ((add >=> mul) 5)
