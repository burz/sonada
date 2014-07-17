module Handler.Modules
( getModulesR
, postModulesR
) where

import Handler.Partials

import Import
import Data.Time

getModulesR :: Handler Html
getModulesR = do
    modules <- runDB $ selectList [] [Desc ModuleCreated]
    let _moduleList = _moduleList' modules True
    defaultLayout $ do
        setTitle "Modules"
        $(widgetFile "modules")

postModulesR :: Handler ()
postModulesR = do
    ModuleResponse c n <- requireJsonBody
    t <- liftIO getCurrentTime
    _ <- runDB . insert $ Module c n t
    sendResponseStatus status201 ("CREATED" :: Text)

