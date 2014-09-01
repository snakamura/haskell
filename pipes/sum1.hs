import Control.Monad (forever)
import Data.Maybe (catMaybes)
import Pipes
import qualified Pipes.Prelude as P

source :: Monad m => Producer Int m ()
source = each [1..9]

test :: Int -> IO ()
test n = runEffect $ (source >-> P.map Just >> forever (yield Nothing)) >->
                     (forever $ sequence (replicate n await) >>= yield) >->
                     P.map catMaybes >->
                     P.takeWhile (not . null) >->
                     P.map sum >->
                     P.print
