import Control.Comonad
    ( (=>>)
    , (=>=)
    , extract
    )
import Control.Comonad.Traced
    ( Traced
    , trace
    , traced
    )

data Object = O
    { value :: Int
    , message :: String
    } deriving Show

add :: Traced String Object -> Object
add t = extract $ t =>>
            trace ("Adding 10 to " ++ show (value $ extract t) ++ "\n") =>>
            \t -> let o = extract t in o { value = value o + 10 }

mul :: Traced String Object -> Object
mul t = extract $ t =>>
            trace ("Multiplying " ++ show (value $ extract t) ++ " by 2\n") =>>
            \t -> let o = extract t in o { value = value o * 2 }

result, result' :: Object
result = extract $ (traced $ \m -> O 5 m) =>> add =>> mul
result' = (add =>= mul) $ traced $ \m -> O 5 m
