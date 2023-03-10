module Parser where

import Data.Functor (($>))
import qualified GHC.Base as Ex.AssocLeft
import Lexer
import Syntax
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

program :: Parser Program
program = Program <$> many globalDeclaration

identifier :: Parser Expression
identifier =
  Identifier
    <$> identifierStr
    <*> getPosition

globalDeclaration :: Parser Declaration
globalDeclaration =
  try globalVariableDeclaration
    <|> try globalFunctionDeclaration

globalVariableDeclaration :: Parser Declaration
globalVariableDeclaration = variableDeclaration

globalFunctionDeclaration :: Parser Declaration
globalFunctionDeclaration = functionDeclaration

variableDeclaration :: Parser Declaration
variableDeclaration =
  VariableDeclaration
    <$> (reserved "var" *> identifier)
    <*> (colon *> typeName)
    <*> getPosition
    <?> "VariableDeclaration"

functionDeclaration :: Parser Declaration
functionDeclaration =
  FunctionDeclaration
    <$> (reserved "func" *> identifier)
    <*> parens (commaSep parameterDeclaration)
    <*> optionMaybe (colon *> typeName)
    <*> (braces block <* reserved "end")
    <*> getPosition
    <?> "FunctionDeclaration"

parameterDeclaration :: Parser Declaration
parameterDeclaration =
  ParameterDeclaration
    <$> identifier
    <*> (colon *> typeName)
    <*> getPosition
    <?> "ParameterDeclaration"

arrayTypeName :: Parser TypeName
arrayTypeName =
  ArrayTypeName
    <$> primitiveTypeName
    <*> many1 (brackets arithExpression)
    <?> "ArrayTypeName"

typeName :: Parser TypeName
typeName =
  try arrayTypeName
    <|> primitiveTypeName
    <?> "TypeName"

primitiveTypeName :: Parser TypeName
primitiveTypeName =
  PrimitiveTypeName
    <$> ( try (reserved "int" $> INT)
            <|> try (reserved "real" $> REAL)
            <?> "PrimitiveTypeName"
        )

block :: Parser Block
block =
  Block
    <$> endBy variableDeclaration semi
    <*> endBy statement semi

statement :: Parser Statement
statement =
  try functionCallStatement
    <|> try assignStatement
    <|> try ifStatement
    <|> try whileStatement
    <|> try returnStatement
    <?> "Statement"

functionCallStatement :: Parser Statement
functionCallStatement =
  FunctionCallStatement
    <$> functionCall
    <*> getPosition
    <?> "FunctionCallStatement"

assignStatement :: Parser Statement
assignStatement =
  AssignStatement
    <$> lvalue
    <*> (reservedOp ":=" *> arithExpression)
    <*> getPosition
    <?> "AssignStatement"

ifStatement :: Parser Statement
ifStatement =
  IfStatement
    <$> (reserved "if" *> condExpression)
    <*> (reserved "then" *> block)
    <*> (optionMaybe (try (reserved "else" >> block)) <* reserved "end")
    <*> getPosition
    <?> "IfStatement"

whileStatement :: Parser Statement
whileStatement =
  WhileStatement
    <$> (reserved "while" *> condExpression)
    <*> (reserved "do" *> block)
    <* reserved "end"
    <*> getPosition
    <?> "WhileStatement"

returnStatement :: Parser Statement
returnStatement =
  ReturnStatement
    <$> (reserved "return" *> optionMaybe arithExpression)
    <*> getPosition
    <?> "ReturnStatement"

lvalue =
  try arrayAccess
    <|> try identifier
    <?> "LValue"

-- binary s f = Ex.Infix (reservedOp s >> return (`BinaryExpression` f))
binary s f = Ex.Infix fun
  where
    fun :: Parser (Expression -> Expression -> Expression)
    fun = do
      reservedOp s
      pos <- getPosition
      return (\e1 e2 -> BinaryExpression e1 f e2 pos)

tableArith =
  [ [ binary "*" MUL Ex.AssocLeft,
      binary "/" DIV Ex.AssocLeft
    ],
    [ binary "+" ADD Ex.AssocLeft,
      binary "-" SUB Ex.AssocLeft
    ]
  ]

tableCond =
  [ [ binary "and" AND Ex.AssocLeft,
      binary "or" OR Ex.AssocLeft
    ]
  ]

tableComp =
  [ [ binary "<" LESS Ex.AssocLeft,
      binary "<=" LESS_EQUALS Ex.AssocLeft,
      binary ">" GREATER Ex.AssocLeft,
      binary ">=" GREATER_EQUALS Ex.AssocLeft,
      binary "==" EQUALS Ex.AssocLeft,
      binary "!=" NOT_EQUALS Ex.AssocLeft
    ]
  ]

arithTerm :: Parser Expression
arithTerm =
  try arrayAccess
    <|> try (Constant <$> numberLiteral <*> getPosition)
    <|> try functionCall
    <|> try identifier
    <|> try castExpr
    <|> try (parens arithExpression)
    <?> "arithmetic term"

compTerm :: Parser Expression
compTerm =
  arithTerm
    <?> "comparison term"

condTerm :: Parser Expression
condTerm =
  try compExpression
    <|> try (parens compExpression)
    <?> "conditional term"

arithExpression :: Parser Expression
arithExpression =
  Ex.buildExpressionParser tableArith arithTerm
    <?> "arithmetic expression"

compExpression :: Parser Expression
compExpression =
  Ex.buildExpressionParser tableComp compTerm
    <?> "comparison expression"

condExpression :: Parser Expression
condExpression =
  Ex.buildExpressionParser tableCond condTerm
    <?> "conditional expression"

castExpr :: Parser Expression
castExpr =
  parens
    ( TypeCast
        <$> arithExpression
        <*> primitiveTypeName
        <* reserved "as"
        <*> getPosition
        <?> "Cast Expression"
    )

variableAccess :: Parser Expression
variableAccess = identifier

numberLiteral :: Parser Literal
numberLiteral =
  try int
    <|> try real
    <|> try Parser.char
    <?> "Number Literal"

int :: Parser Literal
int =
  IntLit . fromInteger
    <$> integer

real :: Parser Literal
real =
  RealLit
    <$> float

char :: Parser Literal
char =
  CharLit
    <$> charLiteral

functionCall :: Parser Expression
functionCall =
  FunctionCall
    <$> identifier
    <*> parens (many argument)
    <*> getPosition
    <?> "Function Call"

argument :: Parser Expression
argument = arithExpression

arrayAccess :: Parser Expression
arrayAccess =
  ArrayAccess
    <$> identifier
    <*> brackets (many arithExpression)
    <*> getPosition
    <?> "Array Access"

-- ENTRY POINTS
contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

{-
parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents arithExpr) "<stdin>"
-}
parseProgram :: SourceName -> String -> Either ParseError Program
parseProgram = parse (contents program)
