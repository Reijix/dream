module IRSyntax where

import Symbol ( Type )

data IRConstant
    = IRIntConstant Int
    | IRRealConstant Double deriving (Show)

data IRVariable = IRVar {
    varName :: String,
    varGlobal :: Bool,
    varVirtualRegister :: Bool,
    varType :: Type
    } deriving (Show)

data IROperand
    = IRVariable IRVariable
    | IRConstant IRConstant deriving (Show)

data IRFunction = IRFunction {
    funName :: String,
    funRetType :: Type,
    funInstructions :: [IRInstruction],
    funParameters :: [IRVariable],
    funLocalVars :: [IRVariable],
    funVirtualRegs :: [IRVariable]
} deriving (Show)

data IRProgram = IRProgram {
    progGlobalVars :: [IRVariable],
    progFunction :: [IRFunction]
} deriving (Show)

newtype LABEL = LBL Int
instance Show LABEL where
    show (LBL num) = show num

data IRInstruction
    = Jump LABEL Jump
    | Assignment Assignment
    | NOP
    | LABEL LABEL
    | STORE IRVariable IROperand IROperand
    | RET (Maybe IROperand)
instance Show IRInstruction where
    show (Jump lbl jmp) = show jmp ++ " " ++ show lbl

data Assignment
    = BinaryOperation IRVariable IROperand IROperand BinaryOperation
    | CastOperation IRVariable IROperand Type Type CastOperation
    | MOV IRVariable IROperand
    | LOAD IRVariable IRVariable IROperand
    | CALL (Maybe IRVariable) String Type [IROperand] deriving (Show)

data CastOperation
    = I2R
    | R2I deriving (Show)

data BinaryOperation
    = ADD
    | SUB
    | MUL
    | DIV deriving (Show)


data Jump 
    = ConditionalJump IROperand IROperand ConditionalJump
    | JMP
instance Show Jump where
    show (ConditionalJump _ _ cnd) = show cnd
    show JMP = "JMP"


data ConditionalJump
    = JEQ
    | JNE
    | JLE
    | JLT
    | JGT
    | JGE deriving (Show)
