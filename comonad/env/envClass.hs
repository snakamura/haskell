{-# LANGUAGE FlexibleContexts #-}

import Control.Comonad
    ( (=>>)
    , (=>=)
    , extract
    )
import Control.Comonad.Env
    ( ComonadEnv
    , ask
    , env
    )

add :: ComonadEnv Int w => w Float -> Float
add env = fromIntegral (ask env) + extract env

mul :: ComonadEnv Int w => w Float -> Float
mul env = fromIntegral (ask env) * extract env

result, result' :: Float
result = extract $ env 3 2 =>> add =>> mul
result' = (add =>= mul) $ env 3 2
