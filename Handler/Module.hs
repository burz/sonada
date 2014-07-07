module Handler.Module where

import Handler.Partials

import Import

getModuleR :: ModuleId -> Handler Html
getModuleR moduleId = do
    mmod <- runDB $ get moduleId
    defaultLayout $ case mmod of
        Nothing  -> do { let code = Nothing
                       ; let _moduleInterface = _moduleInterface' code
                       ; setMessage "Module not found"
                       ; $(widgetFile "homepage")
                       }
        Just md -> do { let code = Just $ moduleCode md
                      ; let name = moduleName md
                      ; let _moduleInterface = _moduleInterface' code
                      ; $(widgetFile "module")
                      }

