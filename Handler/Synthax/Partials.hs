module Handler.Synthax.Partials
( _synthaxInterface'
, _synthaxList'
, _builderInterface'
) where

import Synthax.Parser
import Synthax.JSONGen
import Handler.Partials

import Import
import Data.Text (pack)
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

_builderInterface' :: Maybe (Entity Synthax) -> Widget
_builderInterface' mes = do
    renderUrl <- getUrlRender
    messageRender <- getMessageRender
    addScriptRemote "/static/js/d3.min.js"
    $(widgetFile "Synthax/tools")
    let name = Nothing :: Maybe Text
    let genCodeUrl = renderUrl RenderSynthaxR
    case mes of
        Nothing -> do
            let json = rawJS $ messageRender MsgSynthaxBuilderDefault
            let saveRequestType = "POST" :: Text
            let saveSynthaxUrl = renderUrl SynthaxesR
            $(widgetFile "Synthax/partials/_builderInterface")
        Just (Entity sid s) -> do
            let ee = parseText $ synthaxCode s
            case ee of
                Left e -> invalidArgs [pack $ "Error during parsing: " ++ show e]
                Right es -> do
                    let json = renderJSON' es
                    let saveRequestType = "PUT" :: Text
                    let saveSynthaxUrl = renderUrl $ SynthaxR sid
                    $(widgetFile "Synthax/partials/_builderInterface")

