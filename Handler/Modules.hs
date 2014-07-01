module Handler.Modules where

import Handler.Partials

import Import

getModulesR :: Handler Html
getModulesR = do
    modules <- runDB $ selectList [] []
    let _moduleList = _moduleList' modules
    defaultLayout $(widgetFile "modules")

