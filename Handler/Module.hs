module Handler.Module where

import Handler.Partials

import Import

getModuleR :: ModuleId -> Handler Html
getModuleR moduleId = do
    mmod <- runDB $ get moduleId
    defaultLayout $ case mmod of
        Nothing  -> do { setMessage "Module not found"
                       ; notFound
                       }
        Just md -> do { let name = moduleName md
                      ; let _moduleInterface = _moduleInterface' (Entity moduleId md)
                      ; $(widgetFile "module")
                      }

