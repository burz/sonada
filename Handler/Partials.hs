module Handler.Partials
( _console'
, _moduleList'
, _moduleInterface'
) where

import Model

import Import
import Text.Julius (rawJS)

_console' :: Maybe Text -> Int -> Int -> Int -> Int -> Widget
_console' code top left width height = $(widgetFile "partials/_console")

_moduleList' :: [Entity Module] -> Widget
_moduleList' modules = $(widgetFile "partials/_moduleList")

_moduleInterface' :: Widget -> Enctype -> Maybe Text -> Widget
_moduleInterface' form enctype code = do
    let _console = _console' code 20 10 80 70
    messageRender' <- getMessageRender
    let messageRender = rawJS . messageRender'
    $(widgetFile "partials/_moduleInterface")

