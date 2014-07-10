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

names = words "Source Code Gain Crossfade Filter"
opNames = words "<<< >>>"

lexer = Token.makeTokenParser emptyDef
    { Token.commentStart = "/*"
    , Token.commentEnd = "*/"
    , Token.commentLine = "#" 
    , Token.identStart = letter
    , Token.identLetter = alphaNum <|> char '_' <|> char '\''
    , Token.reservedNames = names
    , Token.reservedOpNames = opNames
    }

identifier = Token.identifier lexer
symbol     = Token.symbol lexer
reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens lexer
float      = Token.float lexer

