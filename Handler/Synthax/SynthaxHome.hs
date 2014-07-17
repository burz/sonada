module Handler.Synthax.SynthaxHome
( getSynthaxHomeR
) where

import Handler.Partials
import Handler.Synthax.Partials

import Import
import Yesod.Auth

getSynthaxHomeR :: Handler Html
getSynthaxHomeR = do
    Entity _ user <- requireAuth
    defaultLayout $ do
        setTitle "Synthax"
        let name = Nothing :: Maybe Text
        let _userInfo = _userInfo' user
        let _synthaxInterface = _synthaxInterface' Nothing
        $(widgetFile "Synthax/synthax")

