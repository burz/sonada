module Handler.Home where

import Handler.Partials

import Import

getHomeR :: Handler Html
getHomeR = do
    let handlerName = "getHomeR" :: Text
        code = Nothing :: Maybe Text
    defaultLayout $ do
        let _console = _console' code 20 20 60 70
        aDomId <- newIdent
        setTitle "sonada"
        $(widgetFile "homepage")

