import Control.Comonad
    ( (=>>)
    , (=>=)
    , extract
    )
import Control.Comonad.Env
    ( Env
    , ask
    , env
    )

add :: Env Int Float -> Float
add env = fromIntegral (ask env) + extract env

mul :: Env Int Float -> Float
mul env = fromIntegral (ask env) * extract env

result, result' :: Float
result = extract $ env 3 2 =>> add =>> mul
result' = (add =>= mul) $ env 3 2
