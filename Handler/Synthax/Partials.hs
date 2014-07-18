module Handler.Synthax.Partials
( _synthaxInterface'
, _synthaxList'
) where

import Handler.Partials

import Import
import Text.Julius

_synthaxInterface' :: Maybe (Entity Synthax) -> Widget
_synthaxInterface' mes = do
    renderUrl <- getUrlRender
    $(widgetFile "Synthax/tools")
    let name = mes >>= \(Entity _ s) -> synthaxName s >>= \t -> Just t
    let code = mes >>= \(Entity _ s) -> Just $ synthaxCode s
    let _console = _console' SynthaxConsole code
    let genCodeUrl = renderUrl RenderSynthaxR
    case mes of
        Nothing -> do
            let saveRequestType = "POST" :: Text
            let saveSynthaxUrl = renderUrl SynthaxesR
            $(widgetFile "Synthax/partials/_synthaxInterface")
        Just (Entity sid _) -> do
            let saveRequestType = "PUT" :: Text
            let saveSynthaxUrl = renderUrl $ SynthaxR sid 
            $(widgetFile "Synthax/partials/_synthaxInterface")

_synthaxList' :: [Entity Synthax] -> Bool -> Bool -> Widget
_synthaxList' synthaxes showCode showLinks = $(widgetFile "Synthax/partials/_synthaxList")

