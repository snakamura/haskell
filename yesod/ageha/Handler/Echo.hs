module Handler.Echo where

import Import

getEchoR :: Text -> Handler RepHtml
getEchoR text = do
    defaultLayout $ do
        $(widgetFile "echo")
