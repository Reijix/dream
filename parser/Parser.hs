module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import Data.Functor ( ($>) )

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax
import GHC.IO.SubSystem (conditional)

-- used for expressions
-- binary s f = Ex.Infix (reservedOp s >> return (BinOp f))
binary name fun = Ex.Infix (do { reservedOp name; return fun })

program :: Parser [Declaration]
program = many globalDeclaration

globalDeclaration :: Parser Declaration
globalDeclaration = try globalVariableDeclaration 
                <|> try globalFunctionDeclaration

variableDeclaration :: Parser VariableDeclaration
variableDeclaration = VariableDeclaration 
    <$> (reserved "var" *> identifier)
    <*> (colon *> typeName)

functionDeclaration :: Parser FunctionDeclaration
functionDeclaration = FunctionDeclaration
    <$> (reserved "func" *> identifier)
    <*> parens (commaSep parameterDeclaration)
    <*> optionMaybe (colon *> typeName)
    <*> (braces block <* reserved "end")

parameterDeclaration :: Parser ParameterDeclaration
parameterDeclaration = ParameterDeclaration
    <$> identifier
    <*> (colon *> typeName)

typeName :: Parser TypeName
typeName = TypeName
    <$> primitiveTypeName
    <*> many (brackets arithmeticExpr)

primitiveTypeName :: Parser PrimitiveTypeName
primitiveTypeName = try (reserved "int" $> INT)
    <|> try (reserved "real" $> REAL)

block :: Parser Block
block = Block
    <$> endBy variableDeclaration semi
    <*> endBy statement semi

statement :: Parser Statement
statement = try functionCallStatement
        <|> try assignStatement
        <|> try ifStatement
        <|> try whileStatement
        <|> try returnStatement

functionCallStatement :: Parser Statement
functionCallStatement = FunctionCallStatement
    <$> functionCall
  

-- TODO lhs needs special parsing for arrays
assignStatement :: Parser Statement
assignStatement = AssignStatement
    <$> lvalue
    <*> (reservedOp ":=" *> arithmeticExpr)

ifStatement :: Parser Statement
ifStatement = IfStatement
    <$> (reserved "if" *> conditionalExpr)
    <*> (reserved "then" *> block)
    <*> optionMaybe (reserved "then" *> block) <* reserved "end"

whileStatement :: Parser Statement
whileStatement = WhileStatement
    <$> (reserved "while" *> conditionalExpr)
    <*> (reserved "do" *> block) <* reserved "end"

returnStatement :: Parser Statement
returnStatement = ReturnStatement
    <$> (reserved "return" *> optionMaybe arithmeticExpr)

lvalue :: Parser LValue
lvalue = try (LValueIdentifier <$> identifier)
     <|> try (LValueArray <$> arrayAccess)

conditionalExpr :: Parser ConditionalExpr
conditionalExpr = orExpr

orExpr :: Parser OrExpr
orExpr = OrExpr
    <$> andExpr
    <*> many (reserved "or" *> andExpr)

andExpr :: Parser AndExpr
andExpr = AndExpr
    <$> compExpr
    <*> many (reserved "and" *> compExpr)

compOp :: Parser CompOp
compOp = try (reservedOp "==" $> EQUALS)
     <|> try (reservedOp "!=" $> NOT_EQUALS)
     <|> try (reservedOp "<" $> LESS)
     <|> try (reservedOp "<=" $> LESS_EQUALS)
     <|> try (reservedOp ">" $> GREATER)
     <|> try (reservedOp ">=" $> GREATER_EQUALS)

compExpr :: Parser CompExpr
compExpr = CompExpr
    <$> arithmeticExpr
    <*> compOp
    <*> arithmeticExpr

arithmeticExpr :: Parser ArithmeticExpr
arithmeticExpr = additiveExpr

additiveOp :: Parser AdditiveOp
additiveOp = try (reservedOp "+" $> ADD)
         <|> try (reservedOp "-" $> SUB)

additiveComplexExpr :: Parser AdditiveExpr
additiveComplexExpr = AdditiveComplexExpr
    <$> additiveExpr
    <*> additiveOp
    <*> additiveSimple

additiveSimple :: Parser AdditiveExpr
additiveSimple = AdditiveSimple
    <$> multiplicativeExpr

additiveExpr :: Parser AdditiveExpr
additiveExpr = try additiveComplexExpr
           <|> try additiveSimple

multiplicativeOp :: Parser MultiplicativeOp
multiplicativeOp = try (reservedOp "*" $> MUL)
               <|> try (reservedOp "/" $> DIV)

multiplicativeComplexExpr :: Parser MultiplicativeExpr
multiplicativeComplexExpr = MultiplicativeComplexExpr
    <$> multiplicativeExpr
    <*> multiplicativeOp
    <*> multiplicativeFactor

multiplicativeFactor :: Parser MultiplicativeExpr
multiplicativeFactor = MultiplicativeFactor 
    <$> factor

multiplicativeExpr :: Parser MultiplicativeExpr
multiplicativeExpr = try multiplicativeComplexExpr
                 <|> try multiplicativeFactor    

factor :: Parser Factor
factor = try (FactorVariableAccess <$> variableAccess)
     <|> try (FactorNumberLiteral <$> numberLiteral)
     <|> try (FactorFunctionCall <$> functionCall)
     <|> try (FactorArrayAccess <$> arrayAccess)
     <|> try (parens (FactorArithmeticExpr <$> arithmeticExpr))
     <|> try (FactorCastExpr <$> castExpr)

castExpr :: Parser CastExpr
castExpr = parens (
    CastExpr 
        <$> arithmeticExpr 
        <*> primitiveTypeName <* reserved "as"
    )

variableAccess :: Parser VariableAccess
variableAccess = identifier

numberLiteral :: Parser NumberLiteral
numberLiteral = try int
            <|> try real
            <|> try Parser.char

int :: Parser NumberLiteral
int = IntLit . fromInteger 
    <$> integer

real :: Parser NumberLiteral
real = RealLit 
    <$> float

char :: Parser NumberLiteral
char = CharLit
    <$> charLiteral

functionCall :: Parser FunctionCall
functionCall = FunctionCall
    <$> identifier
    <*> parens (many argument)

argument :: Parser Argument
argument = arithmeticExpr

arrayAccess :: Parser ArrayAccess
arrayAccess = ArrayAccess
    <$> identifier
    <*> brackets (many arithmeticExpr)

globalVariableDeclaration :: Parser Declaration
globalVariableDeclaration = GlobalVariableDeclaration 
    <$> variableDeclaration

globalFunctionDeclaration :: Parser Declaration
globalFunctionDeclaration = GlobalFunctionDeclaration
    <$> functionDeclaration


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
parseProgram :: String -> Either ParseError [Declaration]
parseProgram = parse (contents program) "<stdin>"