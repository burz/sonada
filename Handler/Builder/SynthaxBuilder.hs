module Handler.Builder.SynthaxBuilder
( getSynthaxBuilderR
) where

import Handler.Partials
import Handler.Builder.Partials

import Import
import Yesod.Auth

getSynthaxBuilderR :: SynthaxId -> Handler Html
getSynthaxBuilderR sid = do
    euser <- requireAuth
    ms <- runDB $ get sid
    defaultLayout $ do
        setTitle "Builder"
        $(widgetFile "Builder/partials/_codeBuilder")
        let _userInfo = _userInfo' euser
        case ms of
            Nothing -> notFound
            Just s -> do
                let _builderInterface = _builderInterface' . Just $ Entity sid s
                $(widgetFile "Builder/builder")
