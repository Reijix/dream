module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc

tableArith = [[binary "*" Times Ex.AssocLeft,
          binary "/" Divide Ex.AssocLeft],
          [binary "+" Plus Ex.AssocLeft,
          binary "-" Minus Ex.AssocLeft]]

-- TODO separate expressions into arithmetic, logical, ...
-- EXPRESSIONS

int :: Parser Expr
int = do
  n <- integer
  return $ Float (fromInteger n)

floating :: Parser Expr
floating = do
  n <- float
  return $ Float n

arithExpr :: Parser Expr
arithExpr = Ex.buildExpressionParser tableArith factor

variable :: Parser Expr
variable = do
  var <- identifier
  return $ Var var

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> variable
      <|> parens arithExpr

-- TYPES 

typenameInt :: Parser PrimitiveTypeName
typenameInt = do
  reserved "int"
  return $ TInt

typenameReal :: Parser PrimitiveTypeName
typenameReal = do
  reserved "real"
  return $ TReal

tType :: Parser PrimitiveTypeName
tType = try typenameInt
     <|> typenameReal

-- DECLARATIONS

variableDeclaration :: Parser VariableDeclaration
variableDeclaration = do
  reserved "var"
  name <- identifier
  colon
  typename <- tType
  return $ VariableDeclaration name typename

parameterDeclaration :: Parser ParameterDeclaration
parameterDeclaration = do
  name <- identifier
  colon
  typename <- tType
  return $ ParameterDeclaration name typename

functionDeclaration :: Parser FunctionDeclaration
functionDeclaration = do
  reserved "func"
  name <- identifier
  params <- parens $ commaSep parameterDeclaration
  -- >* parses ':' first but only returns tType!
  retType <- optionMaybe (colon *> tType)
  body <- braces $ block
  reserved "end"
  return $ FunctionDeclaration name retType params body

globalVariableDeclaration :: Parser GlobalDeclaration
globalVariableDeclaration = do
  decl <- variableDeclaration
  return $ GlobalVariableDeclaration decl

globalFunctionDeclaration :: Parser GlobalDeclaration
globalFunctionDeclaration = do
  fun <- functionDeclaration
  return $ GlobalFunctionDeclaration fun

globalDeclaration :: Parser GlobalDeclaration
globalDeclaration = try globalVariableDeclaration
                 <|> try globalFunctionDeclaration

-- STATEMENTS

functionCallStatement :: Parser Statement
functionCallStatement = do
  name <- identifier
  args <- parens $ commaSep arithExpr
  return $ FunctionCallStatement name args

assignStatement :: Parser Statement
assignStatement = do
  -- TODO lhs needs special parsing for arrays
  lhs <- identifier
  reservedOp ":="
  rhs <- arithExpr
  return $ AssignStatement lhs rhs

returnStatement :: Parser Statement
returnStatement = do
  reserved "return"
  returnValue <- optionMaybe arithExpr
  return $ ReturnStatement returnValue

statement :: Parser Statement
statement = try functionCallStatement
         <|> try assignStatement
         <|> try returnStatement

-- BLOCKS

program :: Parser [GlobalDeclaration]
program = many $ do
  decl <- globalDeclaration
  return decl

block :: Parser Block
block = do
  vars <- endBy variableDeclaration semi
  statements <- endBy statement semi
  return $ Block vars statements

-- ENTRY POINTS

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents arithExpr) "<stdin>" s

parseProgram :: String -> Either ParseError [GlobalDeclaration]
parseProgram s = parse (contents program) "<stdin>" s