module Handler.Home where

import Handler.Partials

import Import

moduleForm :: Html -> MForm Handler (FormResult Module, Widget)
moduleForm = renderDivs $ Module
    <$> areq textField "Code" Nothing
    <*> aopt textField "Title" Nothing

getHomeR :: Handler Html
getHomeR = do
    let handlerName = "getHomeR" :: Text
        code = Nothing :: Maybe Text
    (form, enctype) <- generateFormPost moduleForm
    defaultLayout $ do
        let _moduleInterface = _moduleInterface' form enctype code
        setTitle "sonada"
        $(widgetFile "homepage")

