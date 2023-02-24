module Lexer where

import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+", "-", "*", "/", ":=", "or", "and", "==", "!=", "<", "<=", ">", ">="]
    names = ["def", "var", "int", "real", "func", "end", "return", "as"]
    style =
      emptyDef
        { Tok.commentLine = "#",
          Tok.reservedOpNames = ops,
          Tok.reservedNames = names
        }

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

charLiteral :: Parser Char
charLiteral = Tok.charLiteral lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

semi :: Parser String
semi = Tok.semi lexer

colon :: Parser String
colon = Tok.colon lexer

identifierStr :: Parser String
identifierStr = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
