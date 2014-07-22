module Synthax.JSGen
( jsGen
, jsResponse
, jsonResponse
) where

import Synthax.Algebra
import Synthax.AST
import Synthax.Builders
import Synthax.Wrappers
import qualified Model as Model

import Prelude
import Control.Applicative
import Control.Monad.State hiding (get)
import Data.Aeson (object, (.=), encode)
import Data.Array
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Text.Internal.Builder
import Database.Persist hiding (Filter)
import Import (ContentType, Content, typeJavascript, toContent, typeJson, Handler)
import Text.Julius (Javascript(..))
import Yesod (runDB)

type BufferFile = Builder

type VarMap = Map.Map Text Builder

type BufferHandles = [(Int, BufferFile)]

type ModuleSet = Set.Set Model.ModuleId

data JSResult a = JSResult VarMap BufferHandles ModuleSet Builder a

instance Monad JSResult where
    return a = JSResult Map.empty [] Set.empty (fromString "")  a
    JSResult vm fs ms t a >>= f = let JSResult vm' fs' ms' t' b = f a in
        JSResult (Map.union vm' vm) (fs ++ fs') (Set.union ms ms') (t <> t') b

data MaybeJSResult a = MJR (Maybe (JSResult a))

instance Monad MaybeJSResult where
    return a = MJR . Just $ return a
    MJR Nothing >>= _ = MJR Nothing
    MJR (Just j@(JSResult _ _ _ _ a)) >>= f = let r = f a in case r of
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

alg :: VarMap -> ModuleSet -> JSAlgebra
alg vm ms (Source f) = (\(n, h) -> let l = createJSLabel n in
    jMJR $ JSResult vm [(h, fromText f)] ms (sourceWrapper l h) l)
    <$> bothNew
alg vm ms (Code t) = (\n -> let l = createJSLabel n in
    jMJR $ JSResult vm [] ms (codeWrapper l t) l)
    <$> newLabel
alg vm ms (Module mid) = (\n -> let l = createJSLabel n in
    jMJR $ JSResult vm [] (Set.insert mid ms) (moduleFunctionWrapper l mid) l)
    <$> newLabel
alg vm ms (Var x) = (\_ -> let r = Map.lookup x vm in case r of
    Nothing -> MJR Nothing
    Just l -> jMJR $ JSResult vm [] ms (fromString "") l)
    <$> noLabel
alg vm ms (Gain c v) = (\n j -> let l = createJSLabel n in
    j >>= \s -> jMJR $ JSResult vm [] ms (gainWrapper l s v) l)
    <$> newLabel <*> c
alg vm ms (Crossfade c c' v m) = (\n j j' -> let l = createJSLabel n in
    j >>= \s -> j' >>= \s' -> let wrapper = crossfadeWrapper l s s' v m in
    jMJR $ JSResult vm [] ms wrapper l)
    <$> newLabel <*> c <*> c'
alg vm ms (Filter c t f) = (\n j -> let l = createJSLabel n in
    j >>= \s -> jMJR $ JSResult vm [] ms (filterWrapper l s t f) l)
    <$> newLabel <*> c
alg vm ms (Let i e) = (\n mjr -> case mjr of
    MJR Nothing -> MJR Nothing
    MJR (Just (JSResult m fs ms' c l')) -> let l = createJSLabel n in
        let m' = Map.insert i l (Map.union m vm) in
        jMJR $ JSResult m' fs (Set.union ms ms') (c <> letWrapper l l') l)
    <$> newLabel <*> e

jsGen'' :: VarMap -> ModuleSet -> Fix Expr -> HandleGenerator (MaybeJSResult Builder)
jsGen'' vm ms e = mcata (alg vm ms) e

(~>>) :: HandleGenerator (MaybeJSResult Builder)
         -> Fix Expr
         -> HandleGenerator (MaybeJSResult Builder)
(~>>) r e = do
    mjsr <- r
    case mjsr of
        MJR Nothing -> return $ MJR Nothing
        MJR (Just (JSResult vm _ ms _ _)) -> do
            mjsr' <- jsGen'' vm ms e
            return $ mjsr >>= \_ -> mjsr'

chainJSGen :: [Fix Expr] -> HandleGenerator (MaybeJSResult Builder)
chainJSGen [] = return $ MJR Nothing
chainJSGen (e:es) = foldr (\e' r -> r ~>> e') (jsGen'' Map.empty Set.empty e) es

jsGen' :: [Fix Expr] -> HandleGenerator (Maybe (Builder, ModuleSet, Builder))
jsGen' e = chainJSGen e >>= \mjr -> case mjr of
    MJR Nothing -> return Nothing
    MJR (Just (JSResult _ fs ms j l)) -> numberOfFileHandles >>= \n ->
        let b = bufferWrapper n (array (0, n) fs) in
        return $ Just (b <> j, ms, l)

evalJSGen' :: [Fix Expr] -> Maybe (Builder, ModuleSet, Builder)
evalJSGen' e = evalState (jsGen' e) (0, 0)

makeModule :: Model.ModuleId -> Handler (Maybe Builder) -> Handler (Maybe Builder)
makeModule mid r = do
    mm <- runDB $ get mid
    case mm of
        Nothing -> return Nothing
        Just (Model.Module c _ _ _) -> do
            r' <- r
            case r' of
                Nothing -> return Nothing
                Just b -> return . Just $ b <> moduleCodeWrapper mid c

importModules :: ModuleSet -> Handler (Maybe Builder)
importModules ms = Set.fold makeModule (return $ Just "") ms

link :: [Fix Expr] -> Handler (Maybe (Builder, Builder))
link e = let r = evalJSGen' e in case r of
    Nothing -> return Nothing
    Just (j, ms, l) -> do
        mb <- importModules ms
        case mb of
            Nothing -> return Nothing
            Just b -> return $ Just (b <> j, l)

jsGen :: [Fix Expr] -> Handler (Maybe Javascript)
jsGen e = let r = link e in do
    r' <- r
    case r' of
        Nothing -> return Nothing
        Just (j, _) -> return . Just $ Javascript j

jsResponse :: [Fix Expr] -> Handler (Maybe (ContentType, Content))
jsResponse e = let r = link e in do
    r' <- r
    case r' of
        Nothing -> return Nothing
        Just (j, _) -> return $ Just (typeJavascript, toContent $ toLazyText j)

jsonResponse :: [Fix Expr] -> Handler (Maybe (ContentType, Content))
jsonResponse e = let r = link e in do
    r' <- r
    case r' of
        Nothing -> return Nothing
        Just (j, l) -> let v = object [ "label" .= toLazyText l
                                      , "script" .= toLazyText j
                                      ]
            in return $ Just (typeJson, toContent $ encode v)

