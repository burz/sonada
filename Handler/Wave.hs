module Handler.Wave where

import Handler.Partials

import Import

numberOfModulesToLoad :: Int
numberOfModulesToLoad = 10

getWaveR :: Handler Html
getWaveR = do
    modules <- runDB $ selectList [] [LimitTo numberOfModulesToLoad]
    let _waveChart = _waveChart' modules
    defaultLayout $ do
        setTitle "Wave"
        $(widgetFile "wave")

