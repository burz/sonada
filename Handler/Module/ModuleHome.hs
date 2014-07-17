module Handler.Module.ModuleHome
( getModuleHomeR
) where

import Handler.Partials

import Import

getModuleHomeR :: Handler Html
getModuleHomeR = defaultLayout $ do
    setTitle "Module"
    let name = Nothing :: Maybe Text
    let _moduleInterface = _moduleInterface' Nothing
    $(widgetFile "module")

