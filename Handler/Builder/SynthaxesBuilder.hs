module Handler.Builder.SynthaxesBuilder
( getSynthaxesBuilderR
) where

import Handler.Partials
import Handler.Builder.Partials

import Import
import Yesod.Auth

getSynthaxesBuilderR :: Handler Html
getSynthaxesBuilderR = do
    euser <- requireAuth
    defaultLayout $ do
        setTitle "Builder"
        $(widgetFile "Builder/partials/_codeBuilder")
        let _userInfo = _userInfo' euser
        let _builderInterface = _builderInterface' Nothing
        $(widgetFile "Builder/builder")

