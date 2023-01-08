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

nat :: Parser Expr
nat = Nat . fromInteger <$> integer

real :: Parser Expr
real = Real <$> float

arithExpr :: Parser Expr
arithExpr = Ex.buildExpressionParser tableArith factor

-- TODO change this up to better match specification.
variable :: Parser Expr
variable = Var <$> identifier

factor :: Parser Expr
factor = try real
      <|> try nat
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
     <|> typenameReal

-- DECLARATIONS

variableDeclaration :: Parser VariableDeclaration
variableDeclaration = do
  reserved "var"
  name <- identifier
  colon
  VariableDeclaration name <$> tType

parameterDeclaration :: Parser ParameterDeclaration
parameterDeclaration = do
  name <- identifier
  colon
  ParameterDeclaration name <$> tType

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
globalVariableDeclaration = GlobalVariableDeclaration <$> variableDeclaration

globalFunctionDeclaration :: Parser GlobalDeclaration
globalFunctionDeclaration = GlobalFunctionDeclaration <$> functionDeclaration

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
  AssignStatement lhs <$> arithExpr 

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
program = many globalDeclaration

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
parseExpr = parse (contents arithExpr) "<stdin>"

parseProgram :: String -> Either ParseError [GlobalDeclaration]
parseProgram = parse (contents program) "<stdin>"