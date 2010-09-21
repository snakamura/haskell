import Control.Exception
import Control.Monad
import qualified Database.HDBC as DB
import Database.HDBC.MySQL

main = bracket (connectMySQL defaultMySQLConnectInfo {
                                   mysqlHost = "localhost",
                                   mysqlUnixSocket = "/var/run/mysqld/mysqld.sock",
                                   mysqlUser = "root",
                                   mysqlPassword = "",
                                   mysqlDatabase = "haskell"
                                 })
               DB.disconnect
               process

process :: DB.IConnection conn => conn -> IO ()
process conn =
    do rows <- DB.quickQuery' conn "SELECT * FROM test" []
       forM_ rows print
