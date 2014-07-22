module Synthax.AST
( Expr(..)
) where

import Model

import Prelude
import Data.Text

data Expr a
    = Source Text
    | Code Text
    | Module ModuleId
    | Var Text
    | Gain a Double
    | Crossfade a a Double Double
    | Filter a Text Double
    | Let Text a
    deriving Functor

