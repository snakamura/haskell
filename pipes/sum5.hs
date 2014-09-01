import qualified Control.Foldl as F
import Lens.Family2 (view)
import Pipes
import Pipes.Group
import qualified Pipes.Prelude as P
import Prelude hiding (sum)

source :: Monad m => Producer Int m ()
source = each [1..9]

sum :: (Monad m, Num a) => Int -> Producer a m r -> Producer a m r
sum n = F.purely folds F.sum . view (chunksOf n)

test :: Int -> IO ()
test n = runEffect $ sum n source >-> P.print
