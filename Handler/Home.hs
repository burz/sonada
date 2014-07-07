module Handler.Home where

import Handler.Partials

import Import

getHomeR :: Handler Html
getHomeR = do
    let handlerName = "getHomeR" :: Text
    (md : _) <- runDB $ selectList [] [LimitTo 1]
    defaultLayout $ do
        setTitle "sonada"
        let _moduleInterface = _moduleInterface' md
        $(widgetFile "homepage")

