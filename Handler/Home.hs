module Handler.Home where

import Handler.Partials

import Import

getHomeR :: Handler Html
getHomeR = do
    let handlerName = "getHomeR" :: Text
        code = Nothing :: Maybe Text
    defaultLayout $ do
        setTitle "sonada"
        let _moduleInterface = _moduleInterface' code
        $(widgetFile "homepage")

