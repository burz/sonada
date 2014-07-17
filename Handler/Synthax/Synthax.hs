module Handler.Synthax.Synthax
( getSynthaxR
, putSynthaxR
) where

import Handler.Partials
import Handler.Synthax.Partials

import Import
import Data.Time
import Yesod.Auth

getSynthaxR :: SynthaxId -> Handler Html
getSynthaxR synthaxId = do
    Entity _ user <- requireAuth
    msyn <- runDB $ get synthaxId
    defaultLayout $ case msyn of
        Nothing -> do
            setMessage "Module not found"
            notFound
        Just s -> do
            setTitle "Synthax"
            let name = synthaxName s
            let _userInfo = _userInfo' user
            let _synthaxInterface = _synthaxInterface' . Just $ Entity synthaxId s
            $(widgetFile "Synthax/synthax")

putSynthaxR :: SynthaxId -> Handler Html
putSynthaxR synthaxId = do
    Entity uid _ <- requireAuth
    SynthaxResponse c n <- requireJsonBody
    t <- liftIO $ getCurrentTime
    runDB . replace synthaxId $ Synthax c n uid t
    sendResponseStatus status201 ("UPDATED" :: Text)

