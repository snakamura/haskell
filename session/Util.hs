module Util where

import Control.Monad.Error (MonadError(..))


unfoldM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldM f x = do v <- f x
                 case v of
--                     Just (a, b) -> do r <- unfoldM f b
--                                       return $ a:r
                     Just (a, b) -> unfoldM f b >>= return . (a:)
                     Nothing     -> return []

bracket :: MonadError e m => m a -> (a -> m b) -> (a -> m c) -> m c
bracket before after thing =
    do a <- before
       r <- thing a `catchError` (\e -> after a >> throwError e)
       after a
       return r
