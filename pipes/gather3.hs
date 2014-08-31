import Control.Monad (unless)
import Pipes
import qualified Pipes.Prelude as P

source :: Monad m => Producer Int m ()
source = each [1..9]

gather :: Monad m => Int -> Producer a m r -> Producer [a] m r
gather n producer = go n producer []
  where
    go 0 producer l = do
        yield $ reverse l
        go n producer []
    go m producer l = do
        x <- lift $ next producer
        case x of
            Left r -> do
                unless (null l) $
                    yield $ reverse l
                return r
            Right (v, producer') -> go (m - 1) producer' (v:l)

test :: Int -> IO ()
test n = runEffect $ gather n source >-> P.print
