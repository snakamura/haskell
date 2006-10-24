module Main where

import IO
import qualified Database.HSQL as DB
import qualified Database.HSQL.MySQL as MySQL

host     = "localhost"
db       = "test"
user     = "root"
password = "hogehoge"

main :: IO ()
main = process `DB.catchSql` print
    where
        process = bracket (MySQL.connect host db user password) DB.disconnect proc
        proc conn = do --init conn
                       q conn
        init conn = DB.execute conn "set names utf8"
        q conn = bracket (DB.query conn "select * from test")
                         DB.closeStatement
                         (DB.forEachRow' p)
        p stmt = mapM_ (printColumn stmt) $ first $ unzip3 $ DB.getFieldsTypes stmt
         where
             first (x, _, _) = x
             printColumn stmt name = do v <- DB.getFieldValue stmt name
                                        putStrLn $ v ++ " (" ++ (show $ length v) ++ ")"
