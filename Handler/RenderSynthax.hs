module Handler.RenderSynthax
( postRenderSynthaxR
) where

import Synthax.Parser
import Synthax.JSGen

import Import
import Data.Text (pack)

postRenderSynthaxR :: Handler Html
postRenderSynthaxR = do
    SynthaxResponse c _ <- requireJsonBody
    let r = parseText c
    case r of
        Left e -> invalidArgs [pack $ "Error during parsing: " ++ show e]
        Right e -> do
            let jr = jsonResponse e
            case jr of
                Nothing -> invalidArgs ["Variables must be declared before they are used"]
                Just j -> sendResponse j

