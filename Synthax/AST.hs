module Synthax.AST
( Expr(..)
) where

import Prelude
import Data.Text

data Expr a
    = Source Text
    | Code Text
    | Var Text
    | Gain a Double
    | Crossfade a a Double Double
    | Filter a Text Double
    | Let Text a
    deriving Functor

