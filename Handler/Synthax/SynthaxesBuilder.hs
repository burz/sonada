module Handler.Synthax.SynthaxesBuilder
( getSynthaxesBuilderR
) where

import Handler.Partials
import Handler.Synthax.Partials

import Import
import Yesod.Auth

getSynthaxesBuilderR :: Handler Html
getSynthaxesBuilderR = do
    euser <- requireAuth
    defaultLayout $ do
        setTitle "Builder"
        $(widgetFile "Synthax/partials/_codeBuilder")
        let _userInfo = _userInfo' euser
        let _builderInterface = _builderInterface'
        $(widgetFile "Synthax/builder")

