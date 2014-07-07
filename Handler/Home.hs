module Handler.Home where

import Handler.Partials

import Import

getHomeR :: Handler Html
getHomeR = do
    let handlerName = "getHomeR" :: Text
    defaultLayout $ do
        setTitle "sonada"
        let _moduleInterface = _moduleInterface' Nothing
        $(widgetFile "homepage")

