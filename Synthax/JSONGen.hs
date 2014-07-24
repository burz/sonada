{-# LANGUAGE FlexibleInstances #-}

module Synthax.JSONGen
( renderJSON'
, renderJSON
) where

import Synthax.Algebra
import Synthax.AST
import Synthax.Builders

import Prelude (($), (.))
import Data.Aeson
import Yesod hiding (Filter)

instance ToJSON (Fix Expr) where
    toJSON (Fx (Source f)) = object
        [ "type" .= sid "source"
        , "file" .= f
        ]
    toJSON (Fx (Code c)) = object
        [ "type" .= sid "code"
        , "code" .= c
        ]
    toJSON (Fx (Module mid)) = object
        [ "type" .= sid "module"
        , "moduleId" .= (String $ toPathPiece mid)
        ]
    toJSON (Fx (Var v)) = object
        [ "type" .= sid "var"
        , "name" .= v
        ]
    toJSON (Fx (Gain n v)) = object
        [ "type" .= sid "gain"
        , "child" .= n
        , "value" .= v
        ]
    toJSON (Fx (Crossfade c c' v m)) = object
        [ "type" .= sid "crossfade"
        , "rightChild" .= c
        , "leftChild" .= c'
        , "value" .= v
        , "max" .= m
        ]
    toJSON (Fx (Filter c t v)) = object
        [ "type" .= sid "filter"
        , "child" .= c
        , "filter" .= t
        , "value" .= v
        ]
    toJSON (Fx (Let l e)) = object
        [ "type" .= sid "let"
        , "label" .= l
        , "expr" .= e
        ]

renderJSON' :: [Fix Expr] -> Value
renderJSON' es = object [ "synthax" .= es ]

renderJSON :: [Fix Expr] -> (ContentType, Content)
renderJSON es = (typeJson, toContent . encode $ renderJSON' es)

