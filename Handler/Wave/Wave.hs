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
    euser <- requireAuth
    modules <- runDB $ selectList [] [LimitTo numberOfModulesToLoad]
    defaultLayout $ do
        setTitle "Wave"
        let _waveChart = _waveChart' modules
        let _userInfo = _userInfo' euser
        $(widgetFile "Wave/wave")

