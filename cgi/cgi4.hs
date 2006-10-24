{-# OPTIONS_GHC -fallow-overlapping-instances #-}

import Control.Monad.Trans
import Data.Maybe
import IO

import qualified Database.HDBC as DB
import qualified Database.HDBC.ODBC as ODBC

import Network.NewCGI

import Text.XHtml


cgiMain :: CGI CGIResult
cgiMain = do setHeader "Content-Type" "text/html; charset=utf-8"
             id <- readInput "id"
             case id of
                 Just id -> do rows <- liftIO $ DB.handleSqlError $ process id
                               output $ renderHtml $ page rows
                 Nothing -> output "Invalid ID."
 where
     process :: Int -> IO [[DB.SqlValue]]
     process id = bracket (ODBC.connectODBC "DSN=MySQL-test;") DB.disconnect (selectId id)
     selectId :: Int -> DB.Connection -> IO [[DB.SqlValue]]
     selectId id conn = do stmt <- DB.prepare conn "select * from test where id=?"
                           DB.execute stmt [DB.toSql id]
                           unfoldM fetch stmt
--     fetch stmt = do r <- DB.fetchRow stmt
--                     case r of
--                         Just v  -> do rest <- fetch stmt
--                                       return $ v:rest
--                         Nothing -> return []
{-
     fetch stmt = do r <- DB.fetchRow stmt
                     case r of
                         Just v  -> return $ Just (v, stmt)
                         Nothing -> return Nothing
-}
     fetch stmt = DB.fetchRow stmt >>= (\row -> return $ row >>= \v -> Just (v, stmt))

page :: [[DB.SqlValue]] -> Html
page rows = header << (meta ! [httpequiv "content-type", content "text/html; charset=utf-8"] +++
                       thetitle << "Test") +++
            body << format rows
 where
     format :: [[DB.SqlValue]] -> Html
     format rows = table << map formatRow rows
     formatRow :: [DB.SqlValue] -> Html
     formatRow row = tr << map formatColumn row
     formatColumn :: DB.SqlValue -> Html
     formatColumn column = td << toS column
     toS :: DB.SqlValue -> String
     toS (DB.SqlString s) = s
     toS x = show x

unfoldM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldM f x = do v <- f x
                 case v of
                     Just (a, b) -> do r <- unfoldM f b
                                       return $ a:r
                     Nothing     -> return []

main :: IO ()
main = runCGI (handleErrors cgiMain)
