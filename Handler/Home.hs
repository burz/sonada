module Handler.Home
( getHomeR
) where

import Import

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "sonada"
    $(widgetFile "homepage")

