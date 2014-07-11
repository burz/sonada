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
    $(widgetFile "base/tools")
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

_synthaxInterface' :: Maybe Text -> Widget
_synthaxInterface' code = do
    renderUrl <- getUrlRender
    let name = Nothing :: Maybe Text
    let _console = _console' SynthaxConsole code
    let url = renderUrl RenderSynthaxR
    $(widgetFile "/partials/_synthaxInterface")

