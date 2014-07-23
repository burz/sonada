module Handler.Synthax.SynthaxesBuilder
( getSynthaxesBuilderR
) where

import Handler.Partials

import Import
import Yesod.Auth

getSynthaxesBuilderR :: Handler Html
getSynthaxesBuilderR = do
    euser <- requireAuth
    defaultLayout $ do
        setTitle "Builder"
        let _userInfo = _userInfo' euser
        $(widgetFile "Synthax/builder")

