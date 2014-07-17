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
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import Data.Text.Internal.Builder
import Import (ContentType, Content, typeJavascript, toContent, typeJson)
import Text.Julius (Javascript(..))

type BufferFile = Builder

type VarMap = Map.Map Text Builder

data JSResult a = JSResult VarMap [(Int, BufferFile)] Builder a

instance Monad JSResult where
    return a = JSResult Map.empty [] (fromString "")  a
    JSResult m fs t a >>= f = let JSResult m' fs' t' b = f a in
        JSResult (Map.union m' m) (fs ++ fs') (t <> t') b

data MaybeJSResult a = MJR (Maybe (JSResult a))

instance Monad MaybeJSResult where
    return a = MJR . Just $ return a
    MJR Nothing >>= _ = MJR Nothing
    MJR (Just j@(JSResult _ _ _ a)) >>= f = let r = f a in case r of
        MJR Nothing -> MJR Nothing
        MJR (Just j') -> MJR $ Just (j >>= \_ -> j')

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

type JSAlgebra = MAlgebra HandleGenerator Expr (MaybeJSResult Builder)

createJSLabel :: Int -> Builder
createJSLabel i = "__varNum" .<>. show i

jMJR :: JSResult a -> MaybeJSResult a
jMJR = MJR . Just

alg :: VarMap -> JSAlgebra
alg vm (Source f) = (\(n, h) -> let l = createJSLabel n in
    jMJR $ JSResult vm [(h, fromText f)] (sourceWrapper l h) l)
    <$> bothNew
alg vm (Code t) = (\n -> let l = createJSLabel n in
    jMJR $ JSResult vm [] (codeWrapper l t) l)
    <$> newLabel
alg vm (Var x) = (\_ -> let r = Map.lookup x vm in case r of
    Nothing -> MJR Nothing
    Just l -> jMJR $ JSResult vm [] (fromString "") l)
    <$> noLabel
alg vm (Gain c v) = (\n j -> let l = createJSLabel n in
    j >>= \s -> jMJR $ JSResult vm [] (gainWrapper l s v) l)
    <$> newLabel <*> c
alg vm (Crossfade c c' v m) = (\n j j' -> let l = createJSLabel n in
    j >>= \s -> j' >>= \s' -> let wrapper = crossfadeWrapper l s s' v m in
    jMJR $ JSResult vm [] wrapper l)
    <$> newLabel <*> c <*> c'
alg vm (Filter c t f) = (\n j -> let l = createJSLabel n in
    j >>= \s -> jMJR $ JSResult vm [] (filterWrapper l s t f) l)
    <$> newLabel <*> c
alg vm (Let i e) = (\n mjr -> case mjr of
    MJR Nothing -> MJR Nothing
    MJR (Just (JSResult m fs c l')) -> let l = createJSLabel n in
        let m' = Map.insert i l (Map.union m vm) in
        jMJR $ JSResult m' fs (c <> letWrapper l l') l)
    <$> newLabel <*> e

jsGen'' :: VarMap -> Fix Expr -> HandleGenerator (MaybeJSResult Builder)
jsGen'' vm e = mcata (alg vm) e

(~>>) :: HandleGenerator (MaybeJSResult Builder)
         -> Fix Expr
         -> HandleGenerator (MaybeJSResult Builder)
(~>>) r e = do
    mjsr <- r
    case mjsr of
        MJR Nothing -> return $ MJR Nothing
        MJR (Just (JSResult vm _ _ _)) -> do
            mjsr' <- jsGen'' vm e
            return $ mjsr >>= \_ -> mjsr'

chainJSGen :: [Fix Expr] -> HandleGenerator (MaybeJSResult Builder)
chainJSGen [] = return $ MJR Nothing
chainJSGen (e:es) = foldr (\e' r -> r ~>> e') (jsGen'' Map.empty e) es

jsGen' :: [Fix Expr] -> HandleGenerator (Maybe (Builder, Builder))
jsGen' e = chainJSGen e >>= \mjr -> case mjr of
    MJR Nothing -> return Nothing
    MJR (Just (JSResult _ fs j l)) -> numberOfFileHandles >>= \n ->
        let b = bufferWrapper n (array (0, n) fs) in
        return $ Just (b <> j, l)

evalJSGen' :: [Fix Expr] -> Maybe (Builder, Builder)
evalJSGen' e = evalState (jsGen' e) (0, 0)

jsGen :: [Fix Expr] -> Maybe Javascript
jsGen e = let r = evalJSGen' e in case r of
    Nothing -> Nothing
    Just (j, _) -> Just $ Javascript j

jsResponse :: [Fix Expr] -> Maybe (ContentType, Content)
jsResponse e = let r = evalJSGen' e in case r of
    Nothing -> Nothing
    Just (j, _) -> Just $ (typeJavascript, toContent $ toLazyText j)

jsonResponse :: [Fix Expr] -> Maybe (ContentType, Content)
jsonResponse e = let r = evalJSGen' e in case r of
    Nothing -> Nothing
    Just (j, l) -> let v = object [ "label" .= toLazyText l
                                  , "script" .= toLazyText j
                                  ]
        in Just $ (typeJson, toContent $ encode v)

