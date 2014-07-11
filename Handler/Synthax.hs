module Handler.Synthax
( getSynthaxR
) where

import Synthax.Quoter
import Synthax.JSGen
import Handler.Partials

import Import

getSynthaxR :: Handler Html
getSynthaxR = defaultLayout $ do
    let name = Nothing :: Maybe Text
    let _synthaxInterface = _synthaxInterface' Nothing
    $(widgetFile "synthax")

