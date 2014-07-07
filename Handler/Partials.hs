module Handler.Partials
( _moduleList'
, _moduleInterface'
, _waveChart'
) where

import Model

import Import

_console' :: Maybe Text -> Widget
_console' code = $(widgetFile "partials/_console")

_moduleList' :: [Entity Module] -> Bool -> Widget
_moduleList' modules showLinks = $(widgetFile "partials/_moduleList")

_moduleInterface' :: Entity Module -> Widget
_moduleInterface' (Entity mid md) = do
    addScriptRemote "/static/js/ace-min/ace.js"
    $(widgetFile "base/tools")
    let _console = _console' . Just $ moduleCode md
    $(widgetFile "partials/_moduleInterface")

_chart' :: Widget
_chart' = $(widgetFile "partials/_chart")

_waveChart' :: [Entity Module] -> Widget
_waveChart' modules = do
    addScriptRemote "/static/js/Chart.min.js"
    let _moduleList = _moduleList' modules False
    let _chart = _chart'
    $(widgetFile "partials/_waveChart")

