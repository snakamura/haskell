import Control.Monad (forever)
import Data.Foldable (foldMap)
import Data.Maybe (fromJust, isJust)
import Data.Monoid (Monoid, Sum(Sum), getSum)
import Pipes
import qualified Pipes.Prelude as P
import Prelude hiding (sum)

source :: Monad m => Producer Int m ()
source = each [1..9]

--sum :: Num a => [Maybe a] -> Maybe a
--sum :: (Functor t, Foldable t, Num a) => t (Maybe a) -> Maybe a
sum :: (Functor t, Foldable t, Functor f, Monoid (f (Sum a))) => t (f a) -> f a
sum = fmap getSum . foldMap (fmap Sum)

test :: Int -> IO ()
test n = runEffect $ (source >-> P.map Just >> forever (yield Nothing)) >->
                     (forever $ sequence (replicate n await) >>= yield . sum) >->
                     P.takeWhile isJust >->
                     P.map fromJust >->
                     P.print
