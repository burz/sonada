module Handler.Partials where

import Model

import Import

_console' :: Maybe Text -> Int -> Int -> Int -> Int -> Widget
_console' code top left width height = $(widgetFile "partials/_console")

_moduleList' :: [Entity Module] -> Widget
_moduleList' modules = $(widgetFile "partials/_moduleList")

_moduleInterface' :: Widget -> Enctype -> Maybe Text -> Widget
_moduleInterface' form enctype code = let _console = _console' code 20 10 80 70 in
    $(widgetFile "partials/_moduleInterface")

