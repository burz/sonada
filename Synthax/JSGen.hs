module Synthax.JSGen
( jsGen
, jsResponse
, jsonResponse
) where

import Synthax.Algebra
import Synthax.AST
import Synthax.Builders
import Synthax.Wrappers

import Prelude
import Control.Applicative
import Control.Monad.State
import Data.Aeson (object, (.=), encode)
import Data.Array
import Data.Monoid
import Data.Text.Internal.Builder
import Import (ContentType, Content, typeJavascript, toContent, typeJson)
import Text.Julius (Javascript(..))

type BufferFile = Builder

data JSResult a = JSResult [(Int, BufferFile)] Builder a

instance Monad JSResult where
    return a = JSResult [] (fromText "")  a
    JSResult fs t a >>= f
        = let JSResult fs' t' b = f a in JSResult (fs ++ fs') (t <> t') b

type HandleGenerator = State (Int, Int)

noLabel :: HandleGenerator ()
noLabel = state (\(i, j) -> ((), (i, j)))

newLabel :: HandleGenerator Int
newLabel = state (\(i, j) -> (i, (i + 1, j)))

newFileHandle :: HandleGenerator Int
newFileHandle = state (\(i, j) -> (j, (i, j + 1)))

bothNew :: HandleGenerator (Int, Int)
bothNew = state (\(i, j) -> ((i, j), (i + 1, j + 1)))

numberOfFileHandles :: HandleGenerator Int
numberOfFileHandles = state (\(i, j) -> (j, (i, j)))

type JSAlgebra = MAlgebra HandleGenerator Expr (JSResult Builder)

createJSLabel :: Int -> Builder
createJSLabel i = "__varNum" .<>. show i

alg :: JSAlgebra
alg (Source f) = (\(n, h) -> let l = createJSLabel n in
    JSResult [(h, fromText f)] (sourceWrapper l h) l) <$> bothNew
alg (Code t) = (\n -> let l = createJSLabel n in
    JSResult [] (codeWrapper l t) l) <$> newLabel
alg (Gain c v) = (\n j -> let l = createJSLabel n in
    j >>= \s -> JSResult [] (gainWrapper l s v) l) <$> newLabel <*> c
alg (Crossfade c c' v m) = (\n j j' -> let l = createJSLabel n in
    j >>= \s -> j' >>= \s' -> JSResult [] (crossfadeWrapper l s s' v m) l)
    <$> newLabel <*> c <*> c'
alg (Filter c t f) = (\n j -> let l = createJSLabel n in
    j >>= \s -> JSResult [] (filterWrapper l s t f) l) <$> newLabel <*> c

jsGen'' :: Fix Expr -> HandleGenerator (JSResult Builder)
jsGen'' e = mcata alg e

jsGen' :: Fix Expr -> HandleGenerator (Builder, Builder)
jsGen' e = jsGen'' e >>= \(JSResult fs j l) -> numberOfFileHandles >>= \n ->
    let b = bufferWrapper n (array (0, n) fs) in return (b <> j, l)

evalJsGen' :: Fix Expr -> (Builder, Builder)
evalJsGen' e = evalState (jsGen' e) (0, 0)

jsGen :: Fix Expr -> Javascript
jsGen e = let (j, _) = evalJsGen' e in Javascript j

jsResponse :: Fix Expr -> (ContentType, Content)
jsResponse e = let (j, _) = evalJsGen' e in
    (typeJavascript, toContent $ toLazyText j)

jsonResponse :: Fix Expr -> (ContentType, Content)
jsonResponse e = let (j, l) = evalJsGen' e in
    let v = object
            [ "label" .= toLazyText l
            , "script" .= toLazyText j
            ] in
    (typeJson, toContent $ encode v)

