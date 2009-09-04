{-# LANGUAGE PolymorphicComponents #-}

data Fun = Fun (forall a. a -> a)

f :: Num a => Fun -> Bool -> a -> Bool
f (Fun g) b n = g b && g n /= 0


data Fun2 a = Fun2 (a -> a)

--f2 :: Num a => Fun2 b -> Bool -> a -> Bool
--f2 (Fun2 g) b n = g b && g n /= 0



data V = V (forall a. Num a => a)

g (V a) = showInt a ++ ", " ++ showDouble a


data Num a => V2 a = V2 a

--g2 (V2 a) = showInt a ++ ", " ++ showDouble a


showInt :: Int -> String
showInt = show

showDouble :: Double -> String
showDouble = show
