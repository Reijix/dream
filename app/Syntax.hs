module Syntax where

type Name = String
data Block = Block {
    blockVariableDeclarations :: [VariableDeclaration],
    blockStatements :: [Statement]
} deriving (Eq, Ord, Show)

-- TYPES
-- todo extend types with arraytypes
type Typename = PrimitiveTypeName
data PrimitiveTypeName
    = TInt
    | TReal
    deriving (Eq, Ord, Show)

-- DECLARATIONS
data ParameterDeclaration = ParameterDeclaration {declarationName :: Name,
                                                  declarationType :: Typename}
    deriving (Eq, Ord, Show)

data VariableDeclaration = VariableDeclaration {variableName :: Name,
                                                variableType :: Typename}
    deriving (Eq, Ord, Show)

data FunctionDeclaration = FunctionDeclaration {functionName :: Name,
                                                functionReturnType :: Maybe Typename,
                                                functionParameterDeclarations :: [ParameterDeclaration],
                                                functionBlock :: Block}
    deriving (Eq, Ord, Show)

data GlobalDeclaration
    = GlobalVariableDeclaration VariableDeclaration
    | GlobalFunctionDeclaration FunctionDeclaration
    deriving (Eq, Ord, Show)

-- STATEMENTS
data Statement
    = FunctionCallStatement {
        functionCallName :: Name,
        functionCallArguments :: [Expr]
    }
    | AssignStatement {
        assignLHS :: Name,
        assignRHS :: Expr
    }
    | ReturnStatement {
        returnValue :: Maybe Expr
    } deriving (Eq, Ord, Show)


-- EXPRESSIONS
data Expr
    = Float Double
    | BinOp Op Expr Expr
    | Var String
    | LocalVariable VariableDeclaration
    deriving (Eq, Ord, Show)

data Op
    = Plus
    | Minus
    | Times
    | Divide
    deriving (Eq, Ord, Show)