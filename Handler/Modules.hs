module Handler.Modules where

import Handler.Partials

import Import

getModulesR :: Handler Html
getModulesR = do
    modules <- runDB $ selectList [] []
    let _moduleList = _moduleList' modules
    defaultLayout $ do
        setTitle "Modules"
        $(widgetFile "modules")

postModulesR :: Handler ()
postModulesR = do
    m <- requireJsonBody :: Handler Module
    _ <- runDB $ insert m
    sendResponseStatus status201 ("CREATED" :: Text)

