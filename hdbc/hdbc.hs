import IO
import qualified Database.HDBC as DB
import qualified Database.HDBC.ODBC as ODBC

main :: IO ()
main = process `DB.catchSql` print
--main = do {ODBC.connectODBC "DSN=MySQL-test";return ()} `DB.catchSql` print
 where
     process = bracket (ODBC.connectODBC "DSN=MySQL-test;") DB.disconnect proc
     proc conn = q conn
     q conn = do res <- DB.quickQuery conn "select * from test" []
                 mapM_ (mapM_ p) res
     p (DB.SqlString s) = printWithLen s
     p x = printWithLen $ show x
     printWithLen x = putStrLn $ x ++ " (" ++ (show $ length x) ++ ")"

