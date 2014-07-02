module Forms.Module
( moduleForm
) where

import Forms.Base

import Import hiding (hiddenField)

moduleForm :: Html -> MForm Handler (FormResult Module, Widget)
moduleForm extra = do
    (codeRes, codeView) <- mreq (hiddenField textField) "" Nothing
    (titleRes, titleView) <- mopt textField "Title" Nothing
    let moduleRes = Module <$> codeRes <*> titleRes
    let widget = [whamlet|
        #{extra}
        ^{fvInput codeView}
        ^{fvInput titleView}
    |]
    return (moduleRes, widget)

