module Handler.Partials
( _moduleList'
, _moduleInterface'
, _waveChart'
, _synthaxInterface'
) where

import Model

import Import
import Text.Julius

data ConsoleType = ModuleConsole | SynthaxConsole

_console' :: ConsoleType -> Maybe Text -> Widget
_console' consoleType code = do
    addScriptRemote "/static/js/ace-min/ace.js"
    $(widgetFile "partials/_console")

_moduleList' :: [Entity Module] -> Bool -> Widget
_moduleList' modules showLinks = $(widgetFile "partials/_moduleList")

_moduleInterface' :: Maybe (Entity Module) -> Widget
_moduleInterface' mem = do
    renderUrl <- getUrlRender
    case mem of
        Nothing -> do
            let name = Nothing :: Maybe Text
            let _console = _console' ModuleConsole Nothing
            let method = "POST" :: Text
            let url = renderUrl ModulesR
            $(widgetFile "partials/_moduleInterface")
        Just (Entity mid md) -> do
            let name = moduleName md
            let _console = _console' ModuleConsole . Just $ moduleCode md
            let method = "PUT" :: Text
            let url = renderUrl $ ModuleR mid
            $(widgetFile "partials/_moduleInterface")

_chart' :: Widget
_chart' = $(widgetFile "partials/_chart")

_waveChart' :: [Entity Module] -> Widget
_waveChart' modules = do
    addScriptRemote "/static/js/Chart.min.js"
    let _moduleList = _moduleList' modules False
    let _chart = _chart'
    $(widgetFile "partials/_waveChart")

_synthaxInterface' :: Maybe (Entity Synthax) -> Widget
_synthaxInterface' mes = do
    renderUrl <- getUrlRender
    $(widgetFile "base/tools")
    let name = mes >>= \(Entity _ s) -> synthaxName s >>= \t -> Just t
    let code = mes >>= \(Entity _ s) -> Just $ synthaxCode s
    let _console = _console' SynthaxConsole code
    let genCodeUrl = renderUrl RenderSynthaxR
    case mes of
        Nothing -> do
            let saveRequestType = "POST" :: Text
            let saveSynthaxUrl = renderUrl SynthaxesR
            $(widgetFile "/partials/_synthaxInterface")
        Just (Entity sid _) -> do
            let saveRequestType = "PUT" :: Text
            let saveSynthaxUrl = renderUrl $ SynthaxR sid
            $(widgetFile "/partials/_synthaxInterface")

