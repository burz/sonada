module Handler.Module.Modules
( getModulesR
, postModulesR
) where

import Handler.Partials
import Handler.Module.Partials

import Import
import Data.Time
import Yesod.Auth

getModulesR :: Handler Html
getModulesR = do
    Entity _ user <- requireAuth
    modules <- runDB $ selectList [] [Desc ModuleCreated]
    defaultLayout $ do
        setTitle "Modules"
        let _moduleList = _moduleList' modules True
        let _userInfo = _userInfo' user
        $(widgetFile "Module/modules")

postModulesR :: Handler ()
postModulesR = do
    Entity _ _ <- requireAuth
    ModuleResponse c n <- requireJsonBody
    t <- liftIO getCurrentTime
    _ <- runDB . insert $ Module c n t
    sendResponseStatus status201 ("CREATED" :: Text)

