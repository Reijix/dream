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
  try (globalVariableDeclaration <* semi)
    <|> try globalFunctionDeclaration

globalVariableDeclaration :: Parser Declaration
globalVariableDeclaration = variableDeclaration

globalFunctionDeclaration :: Parser Declaration
globalFunctionDeclaration = functionDeclaration

{- 
e.g.
int x;
real[25] y;
-} 
variableDeclaration :: Parser Declaration
variableDeclaration =
  flip VariableDeclaration
    <$> typeName
    <*> identifier
    <*> getPosition
    <?> "VariableDeclaration"

{-
e.g.
int f (int x, real y) {}
-}
functionDeclaration :: Parser Declaration
functionDeclaration =
  (\t id args blk pos -> FunctionDeclaration id args t blk pos)
    <$> optionMaybe typeName
    <*> identifier
    <*> parens (commaSep parameterDeclaration)
    <*> block
    <*> getPosition
    <?> "FunctionDeclaration"

parameterDeclaration :: Parser Declaration
parameterDeclaration =
  flip ParameterDeclaration
    <$> typeName
    <*> identifier
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
  braces $
  Block
    <$> endBy variableDeclaration semi
    <*> many (primitiveStatement <* semi <|> controlStatement)

controlStatement :: Parser Statement
controlStatement = 
  try ifStatement
    <|> try whileStatement 
    <?> "Control statement"

primitiveStatement :: Parser Statement
primitiveStatement =
  try functionCallStatement
    <|> try assignStatement
    <|> try returnStatement
    <?> "Primitive statement"

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
    <*> (reservedOp "=" *> arithExpression)
    <*> getPosition
    <?> "AssignStatement"

ifStatement :: Parser Statement
ifStatement =
  IfStatement
    <$> (reserved "if" *> condExpression)
    <*> block
    <*> optionMaybe (try (reserved "else" >> block))
    <*> getPosition
    <?> "IfStatement"

whileStatement :: Parser Statement
whileStatement =
  WhileStatement
    <$> (reserved "while" *> condExpression)
    <*> block
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
    flip TypeCast
        <$> parens primitiveTypeName
        <*> arithExpression
        <*> getPosition
        <?> "Cast Expression"

variableAccess :: Parser Expression
variableAccess = identifier

numberLiteral :: Parser Literal
numberLiteral =
  try real
    <|> try int
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
    <*> parens (commaSep argument)
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
