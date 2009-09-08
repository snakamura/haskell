{-# LANGUAGE GADTs #-}

import Data.Sequence

data Vector a where
    BoolVector :: [Bool] -> Vector Bool
    IntVector :: Seq Int -> Vector Int

instance Show (Vector a) where
    show (BoolVector l) = "BoolVector " ++ show l
    show (IntVector s) = "IntVector (" ++ show s ++ ")"

add :: a -> Vector a -> Vector a
add x (BoolVector l) = BoolVector $ x:l
add x (IntVector s) = IntVector $ x <| s
