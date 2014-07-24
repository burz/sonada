module Handler.Builder.RenderSynthaxBuilder
( getRenderSynthaxBuilderR
) where

import Synthax.Parser
import Synthax.JSONGen

import Import
import Data.Text (pack)
import Yesod.Auth

getRenderSynthaxBuilderR :: SynthaxId -> Handler Html
getRenderSynthaxBuilderR sid = do
    _ <- requireAuth
    ms <- runDB $ get sid
    case ms of
        Nothing -> notFound
        Just s -> let r = parseText $ synthaxCode s in
            case r of
                Left e -> invalidArgs [pack $ "Error during parsing: " ++ show e]
                Right es -> sendResponse $ renderJSON es

