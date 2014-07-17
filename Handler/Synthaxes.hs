module Handler.Synthaxes
( getSynthaxesR
, postSynthaxesR
) where

import Handler.Partials

import Import
import Data.Time

getSynthaxesR :: Handler Html
getSynthaxesR = defaultLayout $ do
    setTitle "Synthax"
    let name = Nothing :: Maybe Text
    let _synthaxInterface = _synthaxInterface' Nothing
    $(widgetFile "synthax")

postSynthaxesR :: Handler ()
postSynthaxesR = do
    SynthaxResponse c n <- requireJsonBody
    t <- liftIO $ getCurrentTime
    _ <- runDB $ insert $ Synthax c n t
    sendResponseStatus status201 ("CREATED" :: Text)

