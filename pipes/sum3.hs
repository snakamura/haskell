import Data.Foldable (for_)
import Lens.Family2.State.Strict (zoom)
import Pipes
import Pipes.Parse
import qualified Pipes.Prelude as P
import Prelude hiding (splitAt, sum)

source :: Monad m => Producer Int m ()
source = each [1..9]

sum :: (Monad m, Num a) => Int -> Producer a m r -> Producer a m ()
sum n producer = do
    (m, producer') <- lift $ runStateT parser producer
    for_ m $ \m -> do
        yield m
        sum n producer'
  where
    parser = zoom (splitAt n) $ foldAll add Nothing id
    add (Just x) y = Just $ x + y
    add Nothing y = Just y

test :: Int -> IO ()
test n = runEffect $ sum n source >-> P.print
