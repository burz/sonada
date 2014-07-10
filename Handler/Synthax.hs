module Handler.Synthax
( getSynthaxR
) where

import Synthax.Quoter
import Synthax.JSGen

import Import

getSynthaxR :: Handler Html
getSynthaxR = do
    let c = [synthax|Code <<<alert("it works!!"); function gen(t) { return 0; }>>>|]
    let c' = case c of Right r -> r
    let j = jsGen c'
    defaultLayout $ do
        toWidget j

