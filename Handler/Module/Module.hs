module Handler.Module.Module
( getModuleR
, putModuleR
) where

import Handler.Partials
import Handler.Module.Partials

import Import
import Data.Time
import Yesod.Auth

getModuleR :: ModuleId -> Handler Html
getModuleR moduleId = do
    Entity _ user <- requireAuth
    mmod <- runDB $ get moduleId
    defaultLayout $ case mmod of
        Nothing  -> do
            setMessage "Module not found"
            notFound
        Just md -> do
            let name = moduleName md
            let _userInfo = _userInfo' user
            let _moduleInterface = _moduleInterface' (Just $Entity moduleId md)
            $(widgetFile "Module/module")

putModuleR :: ModuleId -> Handler Html
putModuleR moduleId = do
    Entity uid _ <- requireAuth
    ModuleResponse c n <- requireJsonBody
    t <- liftIO getCurrentTime
    runDB $ replace moduleId $ Module c n uid t
    sendResponseStatus status201 ("UPDATED" :: Text)

