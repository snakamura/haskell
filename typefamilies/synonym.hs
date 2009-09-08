{-# LANGUAGE TypeFamilies #-}

import qualified Data.Sequence as Seq


class List a where
    type Elem a
    empty :: a
    add :: Elem a -> a -> a
    first :: a -> Elem a

instance List [a] where
    type Elem [a] = a
    empty = []
    add = (:)
    first = head

instance List (Seq.Seq a) where
    type Elem (Seq.Seq a) = a
    empty = Seq.empty
    add = (Seq.<|)
    first = flip Seq.index 0

data IntVector = IntVector [Int]

instance List IntVector where
    type Elem IntVector = Int
    empty = IntVector []
    add v (IntVector l) = IntVector $ v:l
    first (IntVector l) = head l

f :: List a => Elem a -> a
f x = add x empty


type family Elem2 a

type instance Elem2 [a] = a

class List2 a where
    empty2 :: a
    add2 :: Elem2 a -> a -> a
    first2 :: a -> Elem2 a

instance List2 [a] where
    empty2 = []
    add2 = (:)
    first2 = head
