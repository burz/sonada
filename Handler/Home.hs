{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
        code = Nothing :: Maybe Text
    defaultLayout $ do
        let _console = _console' code
        aDomId <- newIdent
        setTitle "sonada"
        $(widgetFile "homepage")

_console' :: Maybe Text -> Widget
_console' code = $(widgetFile "_console")
    where deflt = "$(function () { debug(\"lol\"); });" :: Text

