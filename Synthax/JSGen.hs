module Synthax.JSGen
( JSResult(..)
, jsGen
) where

import Synthax.Algebra
import Synthax.AST

import Prelude
import Text.Julius
import Data.Monoid
import Data.Text

data JSResult a = JSResult Javascript a

genRawFromText :: Text -> RawJavascript
genRawFromText = rawJS

genFromText :: Text -> Javascript
genFromText = toJavascript . rawJS

instance Monad JSResult where
    return a = JSResult (genFromText "") a
    JSResult t a >>= f = let JSResult t' b = f a in JSResult (mappend t t') b

type JSAlgebra = MAlgebra JSResult Expr RawJavascript

alg :: JSAlgebra
alg _ = JSResult (genFromText "") (genRawFromText "")

jsGen' :: Fix Expr -> JSResult RawJavascript
jsGen' e = mcata alg e

instance ToJavascript Javascript where
    toJavascript = id

jsGen :: Fix Expr -> JavascriptUrl a
jsGen e = let JSResult j n = jsGen' e in do
    [julius|#{j} lololololol #{n}|]

