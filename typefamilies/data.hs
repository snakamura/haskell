{-# LANGUAGE TypeFamilies, StandaloneDeriving, FlexibleInstances #-}

import Data.Sequence as Seq


class VectorElem a where
    data Vector a
    empty :: Vector a
    add :: a -> Vector a -> Vector a
    first :: Vector a -> a

instance VectorElem Bool where
    data Vector Bool = BoolVector [Bool]
    empty = BoolVector []
    add a (BoolVector v) = BoolVector $ a:v
    first (BoolVector v) = head v

deriving instance Show (Vector Bool)

instance VectorElem Int where
    data Vector Int = IntVector (Seq Int) deriving Show
    empty = IntVector Seq.empty
    add a (IntVector s) = IntVector $ a <| s
    first (IntVector s) = index s 0



data family Vector2 a

data instance Vector2 Bool = BoolVector2 [Bool]

class VectorElem2 a where
    empty2 :: Vector2 a
    add2 :: a -> Vector2 a -> Vector2 a
    first2 :: Vector2 a -> a

instance VectorElem2 Bool where
    empty2 = BoolVector2 []
    add2 a (BoolVector2 v) = BoolVector2 $ a:v
    first2 (BoolVector2 v) = head v
