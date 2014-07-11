module Handler.RenderSynthax
( postRenderSynthaxR
) where

import Synthax.Parser
import Synthax.JSGen

import Import

postRenderSynthaxR :: Handler Html
postRenderSynthaxR = do
    s <- requireJsonBody :: Handler Synthax
    let r = parseText $ synthaxCode s
    let e = case r of Right e' -> e'
    sendResponse $ jsonResponse e

