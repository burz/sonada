module Handler.Synthax.Synthax
( getSynthaxR
, putSynthaxR
) where

import Handler.Partials

import Import
import Data.Time

getSynthaxR :: SynthaxId -> Handler Html
getSynthaxR synthaxId = do
    msyn <- runDB $ get synthaxId
    defaultLayout $ case msyn of
        Nothing -> do
            setMessage "Module not found"
            notFound
        Just s -> do
            setTitle "Synthax"
            let name = synthaxName s
            let _synthaxInterface = _synthaxInterface' . Just $ Entity synthaxId s
            $(widgetFile "synthax")

putSynthaxR :: SynthaxId -> Handler Html
putSynthaxR synthaxId = do
    SynthaxResponse c n <- requireJsonBody
    t <- liftIO $ getCurrentTime
    runDB . replace synthaxId $ Synthax c n t
    sendResponseStatus status201 ("UPDATED" :: Text)

