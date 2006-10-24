import Control.Monad
import Data.List

f :: IO [Char]
f = reverse `liftM` f' []
 where
     f' :: [Char] -> IO [Char]
     f' p = do c <- getChar
               if c == 'x' then return p else f' $ c:p

g :: IO [Char]
g = unfoldM g' 0
 where
     g' _ = do c <- getChar
               return $ if c == 'x' then Nothing else Just (c, 0)

unfoldM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldM f x = do v <- f x
                 case v of
                     Just (a, b) -> unfoldM f b >>= return . (a:)
                     Nothing     -> return []


h :: [Char]
h = takeWhile (/='x') ['a'..]

h' :: IO [Char]
h' = takeWhile (/='x') `liftM` mapM (\_ -> getChar) (cycle [1])
