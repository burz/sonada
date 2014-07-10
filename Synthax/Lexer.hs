module Synthax.Lexer
( names
, opNames
, symbol
, identifier
, reserved
, reservedOp
, parens
, float
) where

import Prelude
import Text.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language
import Text.Parsec.String

names :: [String]
names = words "Source Code Gain Crossfade Filter"

opNames :: [String]
opNames = words "<<< >>>"

lexer :: Token.TokenParser a
lexer = Token.makeTokenParser emptyDef
    { Token.commentStart = "/*"
    , Token.commentEnd = "*/"
    , Token.commentLine = "#" 
    , Token.identStart = letter
    , Token.identLetter = letter <|> char '_'
    , Token.reservedNames = names
    , Token.reservedOpNames = opNames
    }

identifier :: Parser String
identifier = Token.identifier lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

reserved :: String -> Parser ()
reserved   = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

float :: Parser Double
float = Token.float lexer

