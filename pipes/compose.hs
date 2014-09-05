import Data.Functor
import Pipes
import qualified Pipes.Prelude as P

source :: Monad m => Producer Int m ()
source = each [1..9]

target :: (MonadIO m, Show a) => Consumer a m ()
target = P.print


double :: Num a => a -> a
double = (* 2)

plus10 :: Num a => a -> a
plus10 = (+ 10)

test :: IO ()
test = runEffect $ source >-> P.map double >-> P.map plus10 >-> target


yieldMap :: Monad m => (a -> b) -> a -> Producer b m ()
yieldMap f = yield . f

doubleP :: (Monad m, Num a) => a -> Producer a m ()
-- doubleP x = yield $ double x
doubleP = yieldMap double

plus10P :: (Monad m, Num a) => a -> Producer a m ()
-- plus10P x = yield $ plus10 x
plus10P = yieldMap plus10

testP1 :: IO ()
testP1 = runEffect $ for (for source doubleP) plus10P >-> target

testP2 :: IO ()
testP2 = runEffect $ for source (doubleP ~> plus10P) >-> target

testP3 :: IO ()
testP3 = runEffect $ (const source ~> doubleP ~> plus10P) () >-> target


awaitMap :: Monad m => (a -> b) -> Consumer a m b
awaitMap f = f <$> await

doubleC :: (Monad m, Num a) => Consumer a m a
-- doubleC = do
--     x <- await
--     return $ double x
doubleC = awaitMap double

plus10C :: (Monad m, Num a) => Consumer a m a
-- plus10C = do
--     x <- await
--     return $ plus10 x
plus10C = awaitMap plus10

testC :: IO ()
testC = runEffect $ source >-> (doubleC >~ plus10C >~ target)


producerMap :: Monad m => (a -> b) -> Producer a m r -> Producer b m r
producerMap f p = do
    x <- lift $ next p
    case x of
        Left r -> return r
        Right (v, p') -> do
            yield $ f v
            producerMap f p'

doublePP :: (Monad m, Num a) => Producer a m r -> Producer a m r
doublePP = producerMap double

plus10PP :: (Monad m, Num a) => Producer a m r -> Producer a m r
plus10PP = producerMap plus10

testPP :: IO ()
testPP = runEffect $ plus10PP (doublePP source) >-> target
