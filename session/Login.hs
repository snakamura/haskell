{-# OPTIONS_GHC -fallow-overlapping-instances #-}

module Main where

import Data.Maybe (fromMaybe)
import Network.FastCGI (runFastCGIorCGI)
import Network.NewCGI
import Text.XHtml

import Session


defaultPage = "top.cgi"

main :: IO ()
main = runFastCGIorCGI $ handleErrors cgiMain

cgiMain :: CGI CGIResult
cgiMain = do
    setHeader "Content-Type" "text/html; charset=utf-8"
    username <- getInput "username"
    password <- getInput "password"
    redirectUrl <- getInput "redirect"
    l <- login username password
    case l of
        Login _ _ -> do 
            -- Seems that redirect with cookie doesn't work.
            -- redirect $ fromMaybe defaultPage redirectUrl
            output $ renderHtml $ redirectPage $ fromMaybe defaultPage redirectUrl
        Fail         -> output $ renderHtml $ loginPage username redirectUrl "Incorrect username or password."
        AlreadyLogin -> redirect $ fromMaybe defaultPage redirectUrl
        NoCredential -> output $ renderHtml $ loginPage username redirectUrl ""

loginPage :: Maybe String -> Maybe String -> String -> Html
loginPage username redirect message =
    head +++
    body << (
        (if null message then noHtml else p << message) +++
        form ! [method "post", action "login.cgi"] <<
            (table << loginTableContents username +++ redirectInput redirect))
 where
     head = header << (thetitle << "Login" +++
                       meta ! [httpequiv "Content-Type", content "text/html; charset=utf-8"])
     loginTableContents u = usernameRow u `above` passwordRow `above` submitRow
      where
          usernameRow u = (td << "Username") `beside` (td << input ! ([thetype "text", name "username"] ++ userNameValue u))
           where
               userNameValue (Just u) = [value u]
               userNameValue _        = []
          passwordRow = (td << "Password") `beside` (td << input ! [thetype "password", name "password"])
          submitRow = td << input ! [thetype "submit", value "Login"]
     redirectInput (Just r) = input ! [thetype "hidden", name "redirect", value r]
     redirectInput _        = noHtml

redirectPage :: String -> Html
redirectPage url =
    -- TODO
    -- Ensure URL is valid
    -- URL which contains characters like <> may causes XSS.
    -- Non-http URL such as javascript: may casues XSS.
    header << meta ! [httpequiv "refresh", content ("0;url=" ++ url)] +++
    body << noHtml
