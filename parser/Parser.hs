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

globalVariableDeclaration :: Parser Declaration
globalVariableDeclaration = GlobalVariableDeclaration 
    <$> variableDeclaration
    <?> "Global Variable Declaration"

globalFunctionDeclaration :: Parser Declaration
globalFunctionDeclaration = GlobalFunctionDeclaration
    <$> functionDeclaration
    <?> "Global Function Declaration"

variableDeclaration :: Parser VariableDeclaration
variableDeclaration = VariableDeclaration 
    <$> (reserved "var" *> identifier)
    <*> (colon *> typeName)
    <?> "VariableDeclaration"

functionDeclaration :: Parser FunctionDeclaration
functionDeclaration = FunctionDeclaration
    <$> (reserved "func" *> identifier)
    <*> parens (commaSep parameterDeclaration)
    <*> optionMaybe (colon *> typeName)
    <*> (braces block <* reserved "end")
    <?> "FunctionDeclaration"

parameterDeclaration :: Parser ParameterDeclaration
parameterDeclaration = ParameterDeclaration
    <$> identifier
    <*> (colon *> typeName)
    <?> "ParameterDeclaration"

typeName :: Parser TypeName
typeName = TypeName
    <$> primitiveTypeName
    <*> many (brackets arithmeticExpr)
    <?> "TypeName"

primitiveTypeName :: Parser PrimitiveTypeName
primitiveTypeName = try (reserved "int" $> INT)
    <|> try (reserved "real" $> REAL)
    <?> "PrimitiveTypeName"

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

-- TODO lhs needs special parsing for arrays
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

lvalue :: Parser LValue
lvalue = try (LValueIdentifier <$> identifier)
     <|> try (LValueArray <$> arrayAccess)
     <?> "LValue"

conditionalExpr :: Parser ConditionalExpr
conditionalExpr = orExpr

orExpr :: Parser OrExpr
orExpr = OrExpr
    <$> andExpr
    <*> many (reserved "or" *> andExpr)
    <?> "Or-Expression"

andExpr :: Parser AndExpr
andExpr = AndExpr
    <$> compExpr
    <*> many (reserved "and" *> compExpr)
    <?> "And-Expression"

compOp :: Parser CompOp
compOp = try (reservedOp "==" $> EQUALS)
     <|> try (reservedOp "!=" $> NOT_EQUALS)
     <|> try (reservedOp "<" $> LESS)
     <|> try (reservedOp "<=" $> LESS_EQUALS)
     <|> try (reservedOp ">" $> GREATER)
     <|> try (reservedOp ">=" $> GREATER_EQUALS)
     <?> "Compare-Operator"

compExpr :: Parser CompExpr
compExpr = CompExpr
    <$> arithmeticExpr
    <*> compOp
    <*> arithmeticExpr
    <?> "Compare-Expression"

arithmeticExpr :: Parser ArithmeticExpr
arithmeticExpr = additiveExpr

additiveOp :: Parser AdditiveOp
additiveOp = try (reservedOp "+" $> ADD)
         <|> try (reservedOp "-" $> SUB)
         <?> "Additive-Operator"

additiveComplexExpr :: Parser AdditiveExpr
additiveComplexExpr = AdditiveComplexExpr
    <$> additiveExpr
    <*> additiveOp
    <*> additiveSimple
    <?> "Complex Additive Expression"

additiveSimple :: Parser AdditiveExpr
additiveSimple = AdditiveSimple
    <$> multiplicativeExpr
    <?> "Simple additive Expression"

additiveExpr :: Parser AdditiveExpr
additiveExpr = try additiveComplexExpr
           <|> try additiveSimple
           <?> "Additive-Expression"

multiplicativeOp :: Parser MultiplicativeOp
multiplicativeOp = try (reservedOp "*" $> MUL)
               <|> try (reservedOp "/" $> DIV)
               <?> "Multiplicative-Operator"

multiplicativeComplexExpr :: Parser MultiplicativeExpr
multiplicativeComplexExpr = MultiplicativeComplexExpr
    <$> multiplicativeExpr
    <*> multiplicativeOp
    <*> multiplicativeFactor
    <?> "Complex multiplicative Expression"

multiplicativeFactor :: Parser MultiplicativeExpr
multiplicativeFactor = MultiplicativeFactor 
    <$> factor
    <?> "Simple multiplicative Expression"

multiplicativeExpr :: Parser MultiplicativeExpr
multiplicativeExpr = try multiplicativeComplexExpr
                 <|> try multiplicativeFactor
                 <?> "Multiplicative-Expression" 

factor :: Parser Factor
factor = try (FactorVariableAccess <$> variableAccess)
     <|> try (FactorNumberLiteral <$> numberLiteral)
     <|> try (FactorFunctionCall <$> functionCall)
     <|> try (FactorArrayAccess <$> arrayAccess)
     <|> try (parens (FactorArithmeticExpr <$> arithmeticExpr))
     <|> try (FactorCastExpr <$> castExpr)
     <?> "Factor"

castExpr :: Parser CastExpr
castExpr = parens (
    CastExpr 
        <$> arithmeticExpr 
        <*> primitiveTypeName <* reserved "as"
        <?> "Cast Expression"
    )

variableAccess :: Parser VariableAccess
variableAccess = identifier

numberLiteral :: Parser NumberLiteral
numberLiteral = try int
            <|> try real
            <|> try Parser.char
            <?> "Number Literal"

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
    <?> "Function Call"

argument :: Parser Argument
argument = arithmeticExpr

arrayAccess :: Parser ArrayAccess
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
parseProgram :: String -> Either ParseError [Declaration]
parseProgram = parse (contents program) "<stdin>"