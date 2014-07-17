module Handler.Synthax.SynthaxHome
( getSynthaxHomeR
) where

import Handler.Partials

import Import

getSynthaxHomeR :: Handler Html
getSynthaxHomeR = defaultLayout $ do
    setTitle "Synthax"
    let name = Nothing :: Maybe Text
    let _synthaxInterface = _synthaxInterface' Nothing
    $(widgetFile "synthax")

