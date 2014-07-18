module Handler.User.User
( getUserR
) where

import Handler.Partials
import Handler.Module.Partials
import Handler.Synthax.Partials

import Import
import Yesod.Auth

getUserR :: UserId -> Handler Html
getUserR userId = do
    euser <- requireAuth
    modules <- runDB $ selectList [ModuleUser ==. userId] [Desc ModuleCreated]
    synthaxes <- runDB $ selectList [SynthaxUser ==. userId] [Desc SynthaxCreated]
    defaultLayout $ do
        let _userInfo = _userInfo' euser
        let _moduleList = _moduleList' modules False True
        let _synthaxList = _synthaxList' synthaxes False True
        $(widgetFile "User/user")

