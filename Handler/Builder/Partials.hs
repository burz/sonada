module Handler.Builder.Partials
( _builderInterface'
) where

import Synthax.Parser
import Synthax.JSONGen

import Import
import Data.Text (pack)
import Text.Julius

_builderGraph' :: Maybe (Entity Synthax) -> Widget
_builderGraph' msid = do
    renderUrl <- getUrlRender
    case msid of
        Nothing -> do
            let sid = PersistInt64 1
            let synthaxBuilderUrl = rawJS . renderUrl $ RenderSynthaxBuilderR $ Key sid
            $(widgetFile "Builder/partials/_builderGraph")
        Just (Entity sid s) -> do
            let synthaxBuilderUrl = rawJS . renderUrl $ RenderSynthaxBuilderR sid
            $(widgetFile "Builder/partials/_builderGraph")

_builderInterface' :: Maybe (Entity Synthax) -> Widget
_builderInterface' mes = do
    renderUrl <- getUrlRender
    messageRender <- getMessageRender
    addScriptRemote "/static/js/d3.min.js"
    $(widgetFile "Synthax/tools")
    let name = Nothing :: Maybe Text
    let genCodeUrl = renderUrl RenderSynthaxR
    let _builderGraph = _builderGraph' mes
    case mes of
        Nothing -> do
            let json = rawJS $ messageRender MsgSynthaxBuilderDefault
            let saveRequestType = "POST" :: Text
            let saveSynthaxUrl = renderUrl SynthaxesR
            $(widgetFile "Builder/partials/_builderInterface")
        Just (Entity sid s) -> do
            let ee = parseText $ synthaxCode s
            case ee of
                Left e -> invalidArgs [pack $ "Error during parsing: " ++ show e]
                Right es -> do
                    let json = renderJSON' es
                    let saveRequestType = "PUT" :: Text
                    let saveSynthaxUrl = renderUrl $ SynthaxR sid 
                    $(widgetFile "Builder/partials/_builderInterface")

