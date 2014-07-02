module Forms.Module
( moduleForm
) where

import Import hiding (hiddenField)

codeField :: Field Handler Text
codeField = Field
    { fieldParse = fieldParse textField
    , fieldView = \_ _ _ _ _ ->
        [whamlet|
            <input id=_{MsgModuleInputId} type=text style="visibility: hidden;">
        |]
    , fieldEnctype = Multipart
    }

moduleForm :: Html -> MForm Handler (FormResult Module, Widget)
moduleForm extra = do
    (codeRes, codeView) <- mreq codeField "" Nothing
    (titleRes, titleView) <- mopt textField "Title" Nothing
    let moduleRes = Module <$> codeRes <*> titleRes
    let widget = [whamlet|
        #{extra}
        ^{fvInput codeView}
        ^{fvInput titleView}
    |]
    return (moduleRes, widget)

