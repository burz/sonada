module Synthax.JSGen
( JSResult(..)
, jsGen
) where

import Synthax.Algebra
import Synthax.AST

import Prelude
import Control.Applicative
import Control.Monad.State
import Data.Monoid
import Data.Text.Internal.Builder
import Text.Julius

data JSResult a = JSResult [Builder] Builder a

instance Monad JSResult where
    return a = JSResult [] (fromText "")  a
    JSResult fs t a >>= f
        = let JSResult fs' t' b = f a in JSResult (fs ++ fs') (t <> t') b

type HandleGenerator = State Int

noLabel :: HandleGenerator ()
noLabel = state (\i -> ((), i))

newLabel :: HandleGenerator Int
newLabel = state (\i -> (i, i + 1))

type JSAlgebra = MAlgebra HandleGenerator Expr (JSResult Builder)

createJSLabel :: Int -> Builder
createJSLabel i = mappend (fromText "__varNum") . fromString $ show i

alg :: JSAlgebra
alg (Source f) = (\n -> let l = createJSLabel n in
    JSResult [fromText f] (fromText "") l) <$> newLabel
alg _ = (\_ -> JSResult [] (fromText "") (fromText "")) <$> noLabel

jsGen' :: Fix Expr -> HandleGenerator (JSResult Builder)
jsGen' e = mcata alg e

jsGen :: Fix Expr -> Javascript
jsGen e = let JSResult fs j n = evalState (jsGen' e) 0 in Javascript j

