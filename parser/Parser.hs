module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import Data.Functor ( ($>) )

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

-- used for expressions
-- binary s f = Ex.Infix (reservedOp s >> return (BinOp f))
binary name fun = Ex.Infix (do { reservedOp name; return fun })

program :: Parser Program
program = Program <$> many globalDeclaration

identifier :: Parser Expression
identifier = Identifier 
    <$> identifierStr

globalDeclaration :: Parser Declaration
globalDeclaration = try globalVariableDeclaration 
                <|> try globalFunctionDeclaration

globalVariableDeclaration :: Parser Declaration
globalVariableDeclaration = variableDeclaration

globalFunctionDeclaration :: Parser Declaration
globalFunctionDeclaration = functionDeclaration

variableDeclaration :: Parser Declaration
variableDeclaration = VariableDeclaration 
    <$> (reserved "var" *> identifier)
    <*> (colon *> typeName)
    <?> "VariableDeclaration"

functionDeclaration :: Parser Declaration
functionDeclaration = FunctionDeclaration
    <$> (reserved "func" *> identifier)
    <*> parens (commaSep parameterDeclaration)
    <*> optionMaybe (colon *> typeName)
    <*> (braces block <* reserved "end")
    <?> "FunctionDeclaration"

parameterDeclaration :: Parser Declaration
parameterDeclaration = ParameterDeclaration
    <$> identifier
    <*> (colon *> typeName)
    <?> "ParameterDeclaration"

arrayTypeName :: Parser TypeName
arrayTypeName = ArrayTypeName
    <$> primitiveTypeName
    <*> many1 (brackets arithmeticExpr)
    <?> "ArrayTypeName"

typeName :: Parser TypeName
typeName = try arrayTypeName
    <|> primitiveTypeName
    <?> "TypeName"

primitiveTypeName :: Parser TypeName
primitiveTypeName = PrimitiveTypeName
    <$> (try (reserved "int" $> INT)
    <|> try (reserved "real" $> REAL)
    <?> "PrimitiveTypeName")

block :: Parser Block
block = Block
    <$> endBy variableDeclaration semi
    <*> endBy statement semi
    <?> "Block"

statement :: Parser Statement
statement = try functionCallStatement
        <|> try assignStatement
        <|> try ifStatement
        <|> try whileStatement
        <|> try returnStatement
        <?> "Statement"

functionCallStatement :: Parser Statement
functionCallStatement = FunctionCallStatement
    <$> functionCall
    <?> "FunctionCallStatement"

assignStatement :: Parser Statement
assignStatement = AssignStatement
    <$> lvalue
    <*> (reservedOp ":=" *> arithmeticExpr)
    <?> "AssignStatement"

ifStatement :: Parser Statement
ifStatement = IfStatement
    <$> (reserved "if" *> conditionalExpr)
    <*> (reserved "then" *> block)
    <*> optionMaybe (reserved "then" *> block) <* reserved "end"
    <?> "IfStatement"

whileStatement :: Parser Statement
whileStatement = WhileStatement
    <$> (reserved "while" *> conditionalExpr)
    <*> (reserved "do" *> block) <* reserved "end"
    <?> "WhileStatement"

returnStatement :: Parser Statement
returnStatement = ReturnStatement
    <$> (reserved "return" *> optionMaybe arithmeticExpr)
    <?> "ReturnStatement"

lvalue :: Parser Expression
lvalue = try identifier
     <|> try arrayAccess
     <?> "LValue"

conditionalExpr :: Parser Expression
conditionalExpr = orExpr

orExpr :: Parser Expression
orExpr = try orComplexExpr
     <|> orSimpleExpr

orComplexExpr :: Parser Expression
orComplexExpr = BinaryExpression
    <$> andExpr
    <*> (reserved "or" $> OR)
    <*> orExpr
    <?> "Or-Expression"

orSimpleExpr :: Parser Expression
orSimpleExpr = andExpr

andExpr :: Parser Expression
andExpr = try andComplexExpr
      <|> andSimpleExpr

andComplexExpr :: Parser Expression
andComplexExpr = BinaryExpression
    <$> compExpr
    <*> (reserved "and" $> AND)
    <*> andExpr
    <?> "And-Expression"

andSimpleExpr :: Parser Expression
andSimpleExpr = compExpr

compOp :: Parser BinOp
compOp = try (reservedOp "==" $> EQUALS)
     <|> try (reservedOp "!=" $> NOT_EQUALS)
     <|> try (reservedOp "<" $> LESS)
     <|> try (reservedOp "<=" $> LESS_EQUALS)
     <|> try (reservedOp ">" $> GREATER)
     <|> try (reservedOp ">=" $> GREATER_EQUALS)
     <?> "Compare-Operator"

compExpr :: Parser Expression
compExpr = BinaryExpression
    <$> arithmeticExpr
    <*> compOp
    <*> arithmeticExpr
    <?> "Compare-Expression"

arithmeticExpr :: Parser Expression
arithmeticExpr = additiveExpr

additiveOp :: Parser BinOp
additiveOp = try (reservedOp "+" $> ADD)
         <|> try (reservedOp "-" $> SUB)
         <?> "Additive-Operator"

additiveComplexExpr :: Parser Expression
additiveComplexExpr = BinaryExpression
    <$> additiveSimple
    <*> additiveOp
    <*> additiveExpr
    <?> "Complex Additive Expression"

additiveSimple :: Parser Expression
additiveSimple = multiplicativeExpr
    <?> "Simple additive Expression"

additiveExpr :: Parser Expression
additiveExpr = try additiveComplexExpr
           <|> try additiveSimple
           <?> "Additive-Expression"

multiplicativeOp :: Parser BinOp
multiplicativeOp = try (reservedOp "*" $> MUL)
               <|> try (reservedOp "/" $> DIV)
               <?> "Multiplicative-Operator"

multiplicativeComplexExpr :: Parser Expression
multiplicativeComplexExpr = BinaryExpression
    <$> multiplicativeFactor
    <*> multiplicativeOp
    <*> multiplicativeExpr
    <?> "Complex multiplicative Expression"

multiplicativeFactor :: Parser Expression
multiplicativeFactor = factor
    <?> "Simple multiplicative Expression"

multiplicativeExpr :: Parser Expression
multiplicativeExpr = try multiplicativeComplexExpr
                 <|> try multiplicativeFactor
                 <?> "Multiplicative-Expression" 

factor :: Parser Expression
factor = try identifier
     <|> try (Constant <$> numberLiteral)
     <|> try functionCall
     <|> try arrayAccess
     <|> try (parens arithmeticExpr)
     <|> try castExpr
     <?> "Factor"

castExpr :: Parser Expression
castExpr = parens (
    TypeCast
        <$> arithmeticExpr 
        <*> primitiveTypeName <* reserved "as"
        <?> "Cast Expression"
    )

variableAccess :: Parser Expression
variableAccess = identifier

numberLiteral :: Parser Literal
numberLiteral = try int
            <|> try real
            <|> try Parser.char
            <?> "Number Literal"

int :: Parser Literal
int = IntLit . fromInteger 
    <$> integer

real :: Parser Literal
real = RealLit 
    <$> float

char :: Parser Literal
char = CharLit
    <$> charLiteral

functionCall :: Parser Expression
functionCall = FunctionCall
    <$> identifier
    <*> parens (many argument)
    <?> "Function Call"

argument :: Parser Expression
argument = arithmeticExpr

arrayAccess :: Parser Expression
arrayAccess = ArrayAccess
    <$> identifier
    <*> brackets (many arithmeticExpr)
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
parseProgram :: String -> Either ParseError Program
parseProgram = parse (contents program) "<stdin>"