module Handler.Synthax.SynthaxHome
( getSynthaxHomeR
) where

import Handler.Partials
import Handler.Synthax.Partials

import Import
import Yesod.Auth

getSynthaxHomeR :: Handler Html
getSynthaxHomeR = do
    euser <- requireAuth
    defaultLayout $ do
        setTitle "Synthax"
        let name = Nothing :: Maybe Text
        let _userInfo = _userInfo' euser
        let _synthaxInterface = _synthaxInterface' Nothing
        $(widgetFile "Synthax/synthax")

