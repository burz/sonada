module Handler.Module.ModuleHome
( getModuleHomeR
) where

import Handler.Partials
import Handler.Module.Partials

import Import
import Yesod.Auth

getModuleHomeR :: Handler Html
getModuleHomeR = do
    euser <- requireAuth
    defaultLayout $ do
        setTitle "Module"
        let name = Nothing :: Maybe Text
        let _userInfo = _userInfo' euser
        let _moduleInterface = _moduleInterface' Nothing
        $(widgetFile "Module/module")

