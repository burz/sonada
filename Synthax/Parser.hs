module Synthax.Parser
( ParseError
, parseString
, parseText
) where

import Synthax.Algebra
import Synthax.AST
import Synthax.Lexer

import Prelude hiding (filter)
import Control.Applicative ((<*), (*>))
import Data.Text hiding (words, map, filter)
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

value :: ExprParser
value = source
    <|> code
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

parseString :: String -> Either ParseError (Fix Expr)
parseString s = parse (expression <* eof) "" s

parseText :: Text -> Either ParseError (Fix Expr)
parseText t = parse (expression <* eof) "" $ unpack t

