module Handler.Partials where

import Model

import Import

_console' :: Maybe Text -> Int -> Int -> Int -> Int -> Widget
_console' code top left width height = $(widgetFile "partials/_console")
    where deflt = "$(function () { debug(\"lol\"); });" :: Text

_moduleList' :: [Entity Module] -> Widget
_moduleList' modules = $(widgetFile "partials/_moduleList")

