module Handler.Module.Partials
( _moduleList'
, _moduleInterface'
) where

import Handler.Partials

import Import
import Text.Julius

_moduleList' :: [Entity Module] -> Bool -> Bool -> Widget
_moduleList' modules showCode showLinks = $(widgetFile "Module/partials/_moduleList")

_moduleInterface' :: Maybe (Entity Module) -> Widget
_moduleInterface' mem = do
    renderUrl <- getUrlRender
    case mem of
        Nothing -> do
            let name = Nothing :: Maybe Text
            let _console = _console' ModuleConsole Nothing
            let method = "POST" :: Text
            let url = renderUrl ModulesR
            $(widgetFile "Module/partials/_moduleInterface")
        Just (Entity mid md) -> do
            let name = moduleName md
            let _console = _console' ModuleConsole . Just $ moduleCode md
            let method = "PUT" :: Text
            let url = renderUrl $ ModuleR mid 
            $(widgetFile "Module/partials/_moduleInterface")

_chart' :: Widget
_chart' = $(widgetFile "partials/_chart")

