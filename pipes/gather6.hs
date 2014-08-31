import Lens.Family2 (view)
import Pipes
import Pipes.Group
import qualified Pipes.Prelude as P

source :: Monad m => Producer Int m ()
source = each [1..9]

gather :: Monad m => Int -> Producer a m r -> Producer [a] m r
gather n = concats . maps toListProducer . view (chunksOf n)

toListProducer :: Monad m => Producer a m r -> Producer [a] m r
toListProducer producer = go producer []
  where
    go producer l = do
        x <- lift $ next producer
        case x of
            Left r -> do
                yield $ reverse l
                return r
            Right (v, producer') -> go producer' (v:l)

test :: Int -> IO ()
test n = runEffect $ gather n source >-> P.print
