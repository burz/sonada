module Handler.Home
( getHomeR
) where

import Handler.Partials

import Import
import Yesod.Auth

getHomeR :: Handler Html
getHomeR = do
    muser <- maybeAuth
    defaultLayout $ do
        setTitle "sonada"
        let _userInfo = _userInfo'
        $(widgetFile "homepage")

