module Handler.Wave.Partials
( _waveChart'
) where

import Handler.Partials
import Handler.Module.Partials

import Import

_waveChart' :: [Entity Module] -> Widget
_waveChart' modules = do
    addScriptRemote "/static/js/Chart.min.js"
    let _moduleList = _moduleList' modules False
    let _chart = _chart'
    $(widgetFile "Wave/partials/_waveChart")

