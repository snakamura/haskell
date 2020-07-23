{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
    ( (>>=)
    , (>=>)
    , return
    )
import Control.Monad.Reader
    ( MonadReader
    , ask
    , runReader
    )

add :: MonadReader Int m => Float -> m Float
add n = ask >>= \e -> return $ n + fromIntegral e

mul :: MonadReader Int m => Float -> m Float
mul n = ask >>= \e -> return $ n * fromIntegral e

result, result' :: Float
result = runReader (add 2 >>= mul) 3
result' = runReader ((add >=> mul) 2) 3
