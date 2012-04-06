module Handler.Mirror where

import Import
import qualified Data.Text as T

getMirrorR :: Handler RepHtml
getMirrorR = do
    defaultLayout $ do
        $(widgetFile "mirror")

postMirrorR :: Handler RepHtml
postMirrorR = do
    text <- runInputPost $ ireq textField "text"
    defaultLayout $ do
        $(widgetFile "mirrored")
