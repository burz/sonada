module Forms.Base
( hiddenField
) where

import Import hiding (hiddenField)

hiddenField :: Field Handler a -> Field Handler a
hiddenField field = Field
    { fieldParse = fieldParse field
    , fieldView = \idAttr nameAttr otherAttrs _ _ ->
        [whamlet|
            <input id=#{idAttr} name=#{nameAttr} *{otherAttrs} type=text style="visibility: hidden;">
        |]
    , fieldEnctype = Multipart
    }

