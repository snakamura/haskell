import Control.Monad (unless)
import Lens.Family2.State.Strict (zoom)
import Pipes
import Pipes.Parse
import qualified Pipes.Prelude as P
import Prelude hiding (splitAt)

source :: Monad m => Producer Int m ()
source = each [1..9]

gather :: Monad m => Int -> Producer a m r -> Producer [a] m ()
gather n producer = do
    (l, producer') <- lift $ runStateT parser producer
    unless (null l) $ do
        yield l
        gather n producer'
  where
    parser = zoom (splitAt n) drawAll

test :: Int -> IO ()
test n = runEffect $ gather n source >-> P.print
