module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

binary s f = Ex.Infix (reservedOp s >> return (BinOp f))

tableArith = [[binary "*" Times Ex.AssocLeft,
          binary "/" Divide Ex.AssocLeft],
          [binary "+" Plus Ex.AssocLeft,
          binary "-" Minus Ex.AssocLeft]]

-- TODO separate expressions into arithmetic, logical, ...
-- EXPRESSIONS

int :: Parser Expr
int = Int . fromInteger 
    <$> integer

real :: Parser Expr
real = Real 
    <$> float

arithExpr :: Parser Expr
arithExpr = Ex.buildExpressionParser tableArith factor

-- TODO change this up to better match specification.
variable :: Parser Expr
variable = Var 
    <$> identifier

factor :: Parser Expr
factor = try real
      <|> try int
      <|> variable
      <|> parens arithExpr

-- TYPES 

typenameInt :: Parser PrimitiveTypeName
typenameInt = do
  reserved "int"
  return TInt

typenameReal :: Parser PrimitiveTypeName
typenameReal = do
  reserved "real"
  return TReal

tType :: Parser PrimitiveTypeName
tType = try typenameInt
    <|> try typenameReal

-- DECLARATIONS

variableDeclaration :: Parser VariableDeclaration
variableDeclaration = VariableDeclaration 
    <$> (reserved "var" *> identifier)
    <*> (colon *> tType)

parameterDeclaration :: Parser ParameterDeclaration
parameterDeclaration = ParameterDeclaration
    <$> identifier
    <*> (colon *> tType)

functionDeclaration :: Parser FunctionDeclaration
functionDeclaration = FunctionDeclaration
    <$> (reserved "func" *> identifier)
    <*> parens (commaSep parameterDeclaration)
    <*> optionMaybe (colon *> tType)
    <*> (braces block <* reserved "end")
  
globalVariableDeclaration :: Parser GlobalDeclaration
globalVariableDeclaration = GlobalVariableDeclaration 
    <$> variableDeclaration

globalFunctionDeclaration :: Parser GlobalDeclaration
globalFunctionDeclaration = GlobalFunctionDeclaration
    <$> functionDeclaration

globalDeclaration :: Parser GlobalDeclaration
globalDeclaration = try globalVariableDeclaration 
                <|> try globalFunctionDeclaration

-- STATEMENTS

functionCallStatement :: Parser Statement
functionCallStatement = FunctionCallStatement
    <$> identifier
    <*> parens (commaSep arithExpr)
  

-- TODO lhs needs special parsing for arrays
assignStatement :: Parser Statement
assignStatement = AssignStatement
    <$> identifier
    <*> (reservedOp ":=" *> arithExpr)

returnStatement :: Parser Statement
returnStatement = ReturnStatement
    <$> (reserved "return" *> optionMaybe arithExpr)

statement :: Parser Statement
statement = try functionCallStatement
        <|> try assignStatement
        <|> try returnStatement

-- BLOCKS

program :: Parser [GlobalDeclaration]
program = many globalDeclaration

block :: Parser Block
block = Block
    <$> endBy variableDeclaration semi
    <*> endBy statement semi
  
-- ENTRY POINTS

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents arithExpr) "<stdin>"

parseProgram :: String -> Either ParseError [GlobalDeclaration]
parseProgram = parse (contents program) "<stdin>"