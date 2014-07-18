module Handler.Synthax.Synthaxes
( getSynthaxesR
, postSynthaxesR
) where

import Handler.Partials
import Handler.Synthax.Partials

import Import
import Data.Time
import Yesod.Auth

getSynthaxesR :: Handler Html
getSynthaxesR = do
    euser <- requireAuth
    synthaxes <- runDB $ selectList [] [Desc SynthaxCreated]
    defaultLayout $ do
        setTitle "Synthaxes"
        let _userInfo = _userInfo' euser
        let _synthaxList = _synthaxList' synthaxes True True
        $(widgetFile "Synthax/synthaxes")

postSynthaxesR :: Handler ()
postSynthaxesR = do
    Entity uid _ <- requireAuth
    SynthaxResponse c n <- requireJsonBody
    t <- liftIO $ getCurrentTime
    _ <- runDB $ insert $ Synthax c n uid t
    sendResponseStatus status201 ("CREATED" :: Text)

