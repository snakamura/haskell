{-# LANGUAGE InstanceSigs #-}

import Control.Comonad
    ( Comonad
    , (=>>)
    , (=>=)
    , extract
    , extend
    )

newtype Traced w a = Traced (w -> a)

instance Functor (Traced w) where
    fmap f (Traced g) = Traced $ f. g

instance Monoid w => Comonad (Traced w) where
    extract :: Traced w a -> a
    extract (Traced f) = f mempty
    extend :: (Traced w a -> b) -> Traced w a -> Traced w b
    extend f (Traced g) = Traced $ \xw -> f $ Traced (\yw -> g $ xw <> yw)

trace :: w -> Traced w a -> a
trace x (Traced f) = f x


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
result = extract $ (Traced $ \m -> O 5 m) =>> add =>> mul
result' = (add =>= mul) $ Traced $ \m -> O 5 m
