module Synthax.Parser
( ParseError
, parseString
, parseText
) where

import Synthax.Algebra
import Synthax.AST
import Synthax.Lexer
import qualified Model

import Prelude hiding (filter)
import Control.Applicative ((<$>), (<*), (*>))
import Data.Text hiding (words, map, filter)
import Database.Persist.Types (KeyBackend(..), PersistValue(PersistInt64))
import Text.Parsec
import Text.Parsec.String

type ExprParser = Parser (Fix Expr)

source :: ExprParser
source = reserved "Source" *> symbol "\"" *> do
    f <- manyTill anyChar (try $ symbol "\"")
    return . Fx . Source $ pack f

code :: ExprParser
code = reserved "Code" *> reservedOp "<<<" *> do
    js <- manyTill anyChar (try $ reservedOp ">>>")
    return . Fx . Code $ pack js

toModuleId :: Integral i => i -> Model.ModuleId
toModuleId = Key . PersistInt64 . fromIntegral

moduleExpr :: ExprParser
moduleExpr = reserved "Module" *> integer >>= return . Fx . Module . toModuleId

var :: ExprParser
var = Fx . Var . pack <$> identifier

value :: ExprParser
value = source
    <|> code
    <|> moduleExpr
    <|> var
    <|> parens expression

gain :: ExprParser
gain = reserved "Gain" *> do
    c <- value
    f <- float
    return . Fx $ Gain c f

crossfade :: ExprParser
crossfade = reserved "Crossfade" *> do
    c <- value
    c' <- value
    v <- float
    m <- float
    return . Fx $ Crossfade c c' v m

filter :: ExprParser
filter = reserved "Filter" *> do
    c <- value
    let types = words "lowpass highpass bandpass lowshelf peaking notch allpass"
    t <- choice $ map string types
    v <- float
    return . Fx $ Filter c (pack t) v

expression :: ExprParser
expression
      = gain
    <|> crossfade
    <|> filter
    <|> value

letStmt :: ExprParser
letStmt = reserved "Let" *> do
    i <- identifier
    _ <- symbol "="
    e <- expression
    return . Fx $ Let (pack i) e

statement :: ExprParser
statement
      = letStmt
    <|> expression

statements :: Parser [Fix Expr]
statements = semiSep1 statement

parseString :: String -> Either ParseError [Fix Expr]
parseString s = parse (statements <* eof) "" s

parseText :: Text -> Either ParseError [Fix Expr]
parseText t = parse (statements <* eof) "" $ unpack t

