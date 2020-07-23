import Control.Monad
    ( (>>=)
    , (>=>)
    , return
    )
import Control.Monad.Reader
    ( Reader
    , ask
    , runReader
    )

add :: Float -> Reader Int Float
add n = ask >>= \e -> return $ n + fromIntegral e

mul :: Float -> Reader Int Float
mul n = ask >>= \e -> return $ n * fromIntegral e

result, result' :: Float
result = runReader (add 2 >>= mul) 3
result' = runReader ((add >=> mul) 2) 3
