module Handler.Wave.Wave
( getWaveR
) where

import Handler.Partials
import Handler.Wave.Partials

import Import
import Yesod.Auth

numberOfModulesToLoad :: Int
numberOfModulesToLoad = 10

getWaveR :: Handler Html
getWaveR = do
    Entity _ user <- requireAuth
    modules <- runDB $ selectList [] [LimitTo numberOfModulesToLoad]
    defaultLayout $ do
        setTitle "Wave"
        let _waveChart = _waveChart' modules
        let _userInfo = _userInfo' user
        $(widgetFile "Wave/wave")

