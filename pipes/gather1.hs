import Control.Monad (forever)
import Pipes
import qualified Pipes.Prelude as P

source :: Monad m => Producer Int m ()
source = each [1..9]

gather :: Monad m => Int -> Pipe a [a] m r
gather n = forever $ sequence (replicate n await) >>= yield

test :: Int -> IO ()
test n = runEffect $ source >-> gather n >-> P.print
