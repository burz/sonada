module Handler.Home where

import Handler.Partials

import Import

getHomeR :: Handler Html
getHomeR = do
    let handlerName = "getHomeR" :: Text
        code = Nothing :: Maybe Text
    defaultLayout $ do
        let _moduleInterface = _moduleInterface' code
        setTitle "sonada"
        $(widgetFile "homepage")

