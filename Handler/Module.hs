module Handler.Module
( getModuleR
, putModuleR
) where

import Handler.Partials

import Import
import Data.Time

getModuleR :: ModuleId -> Handler Html
getModuleR moduleId = do
    mmod <- runDB $ get moduleId
    defaultLayout $ case mmod of
        Nothing  -> do
            setMessage "Module not found"
            notFound
        Just md -> do
            let name = moduleName md
            let _moduleInterface = _moduleInterface' (Just $Entity moduleId md)
            $(widgetFile "module")

putModuleR :: ModuleId -> Handler Html
putModuleR moduleId = do
    ModuleResponse c n <- requireJsonBody
    t <- liftIO getCurrentTime
    runDB $ replace moduleId $ Module c n t
    sendResponseStatus status201 ("UPDATED" :: Text)

