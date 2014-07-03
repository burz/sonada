module Handler.Wave where

import Handler.Partials

import Import

numberOfModulesToLoad :: Int
numberOfModulesToLoad = 10

getWaveR :: Handler Html
getWaveR = do
    modules <- runDB $ selectList [] [LimitTo numberOfModulesToLoad]
    let _moduleList = _moduleList' modules
    defaultLayout $(widgetFile "wave")

