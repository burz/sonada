module Synthax.AST
( Expr(..)
) where

import Prelude
import Data.Text

data Expr a
    = Source Text
    | Code Text
    | Gain a Float
    | Crossfade a a Float Float
    | Filter a Text Float
    deriving Functor

