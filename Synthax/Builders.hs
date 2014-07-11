{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Synthax.Builders
( BuilderMappend(..)
, sid
, tid
) where

import Prelude
import Data.Monoid
import Data.Text
import Data.Text.Internal.Builder

class BuilderMappend a where
    (.<>) :: a -> Builder -> Builder
    (<>.) :: Builder -> a -> Builder
    (.<>.) :: a -> a -> Builder

instance BuilderMappend String where
    s .<> b = fromString s <> b
    b <>. s = b <> fromString s
    s .<>. s' = fromString s <> fromString s'

instance BuilderMappend Text where
    t .<> b = fromText t <> b
    b <>. t = b <> fromText t
    t .<>. t' = fromText t <> fromText t'

sid :: String -> String
sid = id

tid :: Text -> Text
tid = id

