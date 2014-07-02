module Handler.Home where

import Handler.Partials
import Forms.Module

import Import

getHomeR :: Handler Html
getHomeR = do
    let handlerName = "getHomeR" :: Text
        code = Nothing :: Maybe Text
    (form, enctype) <- generateFormPost moduleForm
    defaultLayout $ do
        let _moduleInterface = _moduleInterface' form enctype code
        setTitle "sonada"
        $(widgetFile "homepage")

