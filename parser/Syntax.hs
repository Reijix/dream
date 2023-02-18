module Syntax where

import Text.Parsec ( SourcePos )

data BinOp
    = EQUALS
    | NOT_EQUALS
    | LESS
    | LESS_EQUALS
    | GREATER
    | GREATER_EQUALS
    | ADD
    | SUB
    | MUL
    | DIV
    | AND
    | OR
    deriving (Eq, Ord, Show)

data PrimitiveType 
    = INT
    | REAL
    deriving (Eq, Ord, Show)

newtype Program = Program { declarations :: [Declaration] }
    deriving (Eq, Ord, Show)

data Block = Block {
    blockDeclarations :: [Declaration],
    blockStatements :: [Statement]
} deriving (Eq, Ord, Show)

data TypeName
    = PrimitiveTypeName PrimitiveType
    | ArrayTypeName { pType :: TypeName, accesses :: [Expression]}
    deriving (Eq, Ord, Show)

data Statement
    = AssignStatement Expression Expression SourcePos
    | FunctionCallStatement Expression SourcePos
    | IfStatement Expression Block (Maybe Block) SourcePos
    | WhileStatement Expression Block SourcePos
    | ReturnStatement (Maybe Expression) SourcePos
    deriving (Eq, Ord, Show)

data Declaration
    = FunctionDeclaration Expression [Declaration] (Maybe TypeName) Block SourcePos
    | ParameterDeclaration Expression TypeName SourcePos
    | VariableDeclaration Expression TypeName SourcePos
    deriving (Eq, Ord, Show)

data Expression
    = ArrayAccess Expression [Expression] SourcePos
    | BinaryExpression Expression BinOp Expression SourcePos
    | Constant Literal SourcePos
    | FunctionCall Expression [Expression] SourcePos
    | Identifier String SourcePos
    | TypeCast Expression TypeName SourcePos
    deriving (Eq, Ord, Show)

data Literal
    = IntLit Int
    | RealLit Double
    | CharLit Char
    deriving (Eq, Ord, Show)

data ASTNode
    = ASTExpression Expression
    | ASTDeclaration Declaration
    | ASTBinOp BinOp
    | ASTLiteral Literal
    | ASTStatement Statement
    | ASTBlock Block
    | ASTTypeName TypeName
    | ASTProgram Program
    deriving (Eq, Ord, Show)
