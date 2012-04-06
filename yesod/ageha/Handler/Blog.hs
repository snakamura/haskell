module Handler.Blog where

import Import
import Yesod.Form.Nic (nicHtmlField)

entryForm :: Form Article
entryForm = renderDivs $ Article <$> areq textField "Title" Nothing
                                 <*> areq nicHtmlField "Content" Nothing

getBlogR :: Handler RepHtml
getBlogR = do
    articles <- runDB $ selectList [] [Desc ArticleTitle]
    ((_, articleWidget), enctype) <- generateFormPost entryForm
    defaultLayout $ do
        $(widgetFile "articles")

postBlogR :: Handler RepHtml
postBlogR = do
    ((res, articleWidget), enctype) <- runFormPost entryForm
    case res of
      FormSuccess article -> do
               articleId <- runDB $ insert article
               setMessage $ toHtml $ (articleTitle article) <> " created"
               redirect $ ArticleR articleId
      _ -> defaultLayout $ do
               setTitle "Error!"
               $(widgetFile "article-error")

getArticleR :: ArticleId -> Handler RepHtml
getArticleR articleId = do
    article <- runDB $ get404 articleId
    defaultLayout $ do
      setTitle $ toHtml $ articleTitle article
      $(widgetFile "article")
