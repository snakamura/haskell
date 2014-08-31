import Control.Monad (unless)
import Data.Functor ((<$>))
import Data.Maybe (catMaybes)
import Pipes
import Pipes.Parse
import qualified Pipes.Prelude as P

source :: Monad m => Producer Int m ()
source = each [1..9]

gather :: (Functor m, Monad m) => Int -> Producer a m r -> Producer [a] m ()
gather n producer = do
    (l, producer') <- lift $ runStateT parser producer
    unless (null l) $ do
        yield l
        gather n producer'
  where
    parser = catMaybes <$> sequence (replicate n draw)

test :: Int -> IO ()
test n = runEffect $ gather n source >-> P.print
