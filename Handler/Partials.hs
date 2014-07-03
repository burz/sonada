module Handler.Partials
( _console'
, _moduleList'
, _moduleInterface'
) where

import Model

import Import

_console' :: Maybe Text -> Widget
_console' code = $(widgetFile "partials/_console")

_moduleList' :: [Entity Module] -> Widget
_moduleList' modules = $(widgetFile "partials/_moduleList")

_moduleInterface' :: Maybe Text -> Widget
_moduleInterface' code = do
    let _console = _console' code
    $(widgetFile "partials/_moduleInterface")

