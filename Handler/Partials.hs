module Handler.Partials
( _userInfo'
, ConsoleType(..)
, _console'
, _chart'
) where

import Import
import Yesod.Auth

_userInfo' :: Entity User -> Widget
_userInfo' (Entity uid user) = $(widgetFile "partials/_userInfo")

data ConsoleType = ModuleConsole | SynthaxConsole

_console' :: ConsoleType -> Maybe Text -> Widget
_console' consoleType code = do
    addScriptRemote "/static/js/ace-min/ace.js"
    $(widgetFile "partials/_console")

_chart' :: Widget
_chart' = $(widgetFile "partials/_chart")

