module Handler.Partials
( _moduleList'
, _moduleInterface'
, _waveChart'
) where

import Model

import Import

_console' :: Maybe Text -> Widget
_console' code = $(widgetFile "partials/_console")

_moduleList' :: [Entity Module] -> Widget
_moduleList' modules = $(widgetFile "partials/_moduleList")

_moduleInterface' :: Maybe Text -> Widget
_moduleInterface' code = do
    addScriptRemote "/static/js/ace-min/ace.js"
    let _console = _console' code
    $(widgetFile "partials/_moduleInterface")

_chart' :: Widget
_chart' = $(widgetFile "partials/_chart")

_waveChart' :: [Entity Module] -> Widget
_waveChart' modules = do
    addScriptRemote "/static/js/Chart.min.js"
    let _moduleList = _moduleList' modules
    let _chart = _chart'
    $(widgetFile "partials/_waveChart")

