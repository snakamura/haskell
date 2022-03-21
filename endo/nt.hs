import Data.Functor.Const
import Data.Functor.Identity
import qualified Data.List.NonEmpty as NE

length' :: [] a -> Const Int a
length' = Const . length

head' :: NE.NonEmpty a -> Identity a
head' = Identity . NE.head
