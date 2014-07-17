module Handler.Synthax.Synthaxes
( getSynthaxesR
, postSynthaxesR
) where

import Handler.Partials

import Import
import Data.Time

getSynthaxesR :: Handler Html
getSynthaxesR = do
    synthaxes <- runDB $ selectList [] [Desc SynthaxCreated]
    let _synthaxList = _synthaxList' synthaxes True
    defaultLayout $ do
        setTitle "Synthaxes"
        $(widgetFile "synthaxes")

postSynthaxesR :: Handler ()
postSynthaxesR = do
    SynthaxResponse c n <- requireJsonBody
    t <- liftIO $ getCurrentTime
    _ <- runDB $ insert $ Synthax c n t
    sendResponseStatus status201 ("CREATED" :: Text)

