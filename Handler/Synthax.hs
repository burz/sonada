module Handler.Synthax
( getSynthaxR
) where

import Handler.Partials

import Import

getSynthaxR :: Handler Html
getSynthaxR = defaultLayout $ do
    setTitle "Synthax"
    let name = Nothing :: Maybe Text
    let _synthaxInterface = _synthaxInterface' Nothing
    $(widgetFile "synthax")

