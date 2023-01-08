module Syntax where

type Program = [Declaration]

data Declaration
    = GlobalVariableDeclaration VariableDeclaration
    | GlobalFunctionDeclaration FunctionDeclaration
    deriving (Eq, Ord, Show)

data VariableDeclaration = VariableDeclaration {variableName :: Identifier,
                                                variableType :: TypeName}
    deriving (Eq, Ord, Show)

data FunctionDeclaration = FunctionDeclaration {functionName :: Identifier,
                                                functionParameterDeclarations :: [ParameterDeclaration],
                                                functionReturnType :: Maybe TypeName,
                                                functionBlock :: Block}
    deriving (Eq, Ord, Show)

data ParameterDeclaration = ParameterDeclaration {declarationName :: Identifier,
                                                  declarationType :: TypeName}
    deriving (Eq, Ord, Show)

data TypeName = TypeName {
    tnPrimitiveTypeName :: PrimitiveTypeName,
    arrayAccesses :: [ArithmeticExpr]
    } deriving (Eq, Ord, Show)

data PrimitiveTypeName
    = INT
    | REAL
    deriving (Eq, Ord, Show)

data Block = Block {
    blockVariableDeclarations :: [VariableDeclaration],
    blockStatements :: [Statement]
} deriving (Eq, Ord, Show)

data Statement
    = FunctionCallStatement FunctionCall
    | AssignStatement {
        assignLHS :: LValue,
        assignRHS :: ArithmeticExpr
    }
    | IfStatement {
        ifCondition :: ConditionalExpr,
        thenBlock :: Block,
        elseBlock :: Maybe Block
    }
    | WhileStatement {
        whileCondition :: ConditionalExpr,
        doBlock :: Block
    }
    | ReturnStatement {
        returnValue :: Maybe ArithmeticExpr
    } deriving (Eq, Ord, Show)

type Identifier = String

data LValue = LValueIdentifier Identifier
            | LValueArray ArrayAccess
    deriving (Eq, Ord, Show)

type ConditionalExpr = OrExpr

data OrExpr = OrExpr AndExpr [AndExpr]
    deriving (Eq, Ord, Show)

data AndExpr = AndExpr CompExpr [CompExpr]
    deriving (Eq, Ord, Show)

data CompOp 
    = EQUALS
    | NOT_EQUALS
    | LESS
    | LESS_EQUALS
    | GREATER
    | GREATER_EQUALS
    deriving (Eq, Ord, Show)

data CompExpr = CompExpr {
    comp_lop :: ArithmeticExpr,
    compExprOp :: CompOp,
    comp_rop :: ArithmeticExpr
} deriving (Eq, Ord, Show)

type ArithmeticExpr = AdditiveExpr

data AdditiveOp 
    = ADD
    | SUB
    deriving (Eq, Ord, Show)

data AdditiveExpr 
    = AdditiveComplexExpr {
    add_lop :: AdditiveExpr,
    addOp :: AdditiveOp,
    add_rop :: AdditiveExpr
    } 
    | AdditiveSimple MultiplicativeExpr
    deriving (Eq, Ord, Show)

data MultiplicativeOp
    = MUL
    | DIV
    deriving (Eq, Ord, Show)

data MultiplicativeExpr 
    = MultiplicativeComplexExpr {
    mul_lop :: MultiplicativeExpr,
    mulOp :: MultiplicativeOp,
    mul_rop :: MultiplicativeExpr
    } 
    | MultiplicativeFactor Factor
    deriving (Eq, Ord, Show)

data Factor = FactorVariableAccess VariableAccess
            | FactorNumberLiteral  NumberLiteral
            | FactorFunctionCall FunctionCall
            | FactorArrayAccess ArrayAccess
            | FactorArithmeticExpr ArithmeticExpr
            | FactorCastExpr CastExpr
    deriving (Eq, Ord, Show)

data CastExpr = CastExpr {
    castedExpression :: ArithmeticExpr,
    castType :: PrimitiveTypeName
} deriving (Eq, Ord, Show)

type VariableAccess = Identifier

data NumberLiteral 
    = IntLit Int
    | RealLit Double
    | CharLit Char
    deriving (Eq, Ord, Show)

data FunctionCall = FunctionCall {
    functionCallName :: Identifier,
    functionCallArguments :: [Argument]
} deriving (Eq, Ord, Show)

type Argument = ArithmeticExpr

data ArrayAccess = ArrayAccess {
    arrayIdentifier :: Identifier,
    arraySizeExpressions :: [ArithmeticExpr]
} deriving (Eq, Ord, Show)
