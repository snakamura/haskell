{-# OPTIONS_GHC -fallow-overlapping-instances #-}

module Session where

import qualified Database.HDBC as DB
import qualified Database.HDBC.ODBC as ODBC
import Network.NewCGI
import SHA1 (sha1)
import System.Random (randomIO)
import System.Time (ClockTime(), TimeDiff(..), addToClockTime, getClockTime, toUTCTime)

import Util


loginPageURI  = "login.cgi"
dsn           = "DSN=MySQL-session;"
sessionPeriod = TimeDiff {
    tdYear    = 0,
    tdMonth   = 0,
    tdDay     = 14,
    tdHour    = 0,
    tdMin     = 0,
    tdSec     = 0,
    tdPicosec = 0
}


data Login = Login String ClockTime
           | Fail
           | AlreadyLogin
           | NoCredential

login :: (Maybe String) -> (Maybe String) -> CGI Login
login username password = do
    sessionId <- getCookie "id"
    l <- liftIO $ DB.handleSqlError $ bracket (ODBC.connectODBC dsn)
                                              DB.disconnect
                                              (proc sessionId)
    case l of
        Login sessionId expire -> do
            setCookie $ createCookie sessionId expire
        _ -> return ()
    return l
 where
     proc (Just sessionId) conn = do
         info <- getUserInfo conn sessionId
         case info of
             Just _  -> return AlreadyLogin
             Nothing -> realLogin conn username password
     proc _ conn = realLogin conn username password
     realLogin :: DB.Connection -> Maybe String -> Maybe String -> IO Login
     realLogin conn (Just u) (Just p) =
         do userId <- check conn u p
            case userId of
                Just userId -> addSessionId conn userId >>= return . uncurry Login
                Nothing     -> return Fail
     realLogin _ _ _ = return NoCredential
     check :: DB.Connection -> String -> String -> IO (Maybe UserId)
     check conn username password =
         do stmt <- DB.prepare conn sql
            DB.execute stmt [DB.toSql username, DB.toSql password]
            r <- DB.fetchRow stmt
            case r of
                Just [n] -> return $ Just (DB.fromSql n)
                _        -> return Nothing
      where
          sql = "SELECT id FROM user WHERE username=? and password=md5(?)"
     createCookie :: String -> ClockTime -> Cookie
     createCookie id expire = Cookie "id" id (Just $ toUTCTime expire) Nothing Nothing False

logout :: CGI ()
logout = do
    sessionId <- getCookie "id"
    case sessionId of
        Just sid -> liftIO $ DB.handleSqlError $ bracket (ODBC.connectODBC dsn)
                                                         DB.disconnect
                                                         (flip removeSessionId sid)
        Nothing -> return ()
    deleteCookie $ newCookie "id" ""

withLogin :: (UserInfo -> DB.Connection -> CGI CGIResult) -> CGI CGIResult
withLogin f = do
    sessionId <- getCookie "id"
    case sessionId of
        Just sid -> do
            bracket (liftIO $ ODBC.connectODBC dsn)
                    (liftIO . DB.disconnect)
                    (withConnection sid)
                `catchCGI` handleSqlException
        Nothing -> redirectToLogin
 where
     withConnection sessionId conn = do
         userInfo <- liftIO $ getUserInfo conn sessionId
         case userInfo of
             Just info -> f info conn
             Nothing   -> redirectToLogin
     redirectToLogin = do
         uri <- getVar "REQUEST_URI"
         case uri of
             Just u  -> redirect (loginPageURI ++ "?" ++ formEncode [("redirect", u)])
             Nothing -> redirect loginPageURI

handleSqlException e = case DB.sqlExceptions e of
                           Just err -> fail ("SQL error:" ++ show err)
                           Nothing  -> throwCGI e


type UserId = Int

data UserInfo = UserInfo {
    userId   :: UserId,
    userName :: String,
    fullName :: Maybe String
} deriving Show

displayName :: UserInfo -> String
displayName (UserInfo _ _        (Just fullname)) = fullname
displayName (UserInfo _ username _              ) = username


getUserInfo :: DB.Connection -> String -> IO (Maybe UserInfo)
getUserInfo conn sessionId =
    do stmt <- DB.prepare conn sql
       now <- getClockTime
       DB.execute stmt [DB.toSql sessionId, DB.toSql now]
       r <- DB.fetchRow stmt
       case r of
           Just [id, u, f] -> return $ Just $ UserInfo (DB.fromSql id) (DB.fromSql u) (get f)
           _               -> return Nothing
 where
     sql = "SELECT user.id, user.username, user.fullname \
                 \ FROM user, session \
                 \ WHERE session.id=? \
                 \   AND session.userid=user.id \
                 \   AND session.expire>?"
     get (DB.SqlString s) = Just s
     get DB.SqlNull       = Nothing

addSessionId :: DB.Connection -> UserId -> IO (String, ClockTime)
addSessionId conn userId =
    do sessionId <- (randomIO :: IO Int) >>= return . sha1 . show
       now <- getClockTime
       let expire = addToClockTime sessionPeriod now 
       stmt <- DB.prepare conn sql
       DB.execute stmt [DB.toSql sessionId, DB.toSql userId, DB.toSql expire]
       return (sessionId, expire)
 where
     sql = "INSERT INTO session VALUES (?, ?, ?)"

removeSessionId :: DB.Connection -> String -> IO ()
removeSessionId conn sessionId = do
    stmt <- DB.prepare conn sql
    DB.execute stmt [DB.toSql sessionId]
    return ()
 where
     sql = "DELETE FROM session WHERE id=?"
