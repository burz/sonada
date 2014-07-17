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
    Entity _ user <- requireAuth
    synthaxes <- runDB $ selectList [] [Desc SynthaxCreated]
    defaultLayout $ do
        setTitle "Synthaxes"
        let _userInfo = _userInfo' user
        let _synthaxList = _synthaxList' synthaxes True
        $(widgetFile "Synthax/synthaxes")

postSynthaxesR :: Handler ()
postSynthaxesR = do
    Entity _ _ <- requireAuth
    SynthaxResponse c n <- requireJsonBody
    t <- liftIO $ getCurrentTime
    _ <- runDB $ insert $ Synthax c n t
    sendResponseStatus status201 ("CREATED" :: Text)
