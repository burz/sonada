module Handler.Synthax
( putSynthaxR
) where

import Import
import Data.Time

putSynthaxR :: SynthaxId -> Handler Html
putSynthaxR synthaxId = do
    SynthaxResponse c n <- requireJsonBody
    t <- liftIO $ getCurrentTime
    runDB . replace synthaxId $ Synthax c n t
    sendResponseStatus status201 ("UPDATED" :: Text)

