module IRSyntax where

import Symbol ( Type )

data IRConstant
    = IRIntConstant Int
    | IRRealConstant Double

data IRVariable = IRVar {
    varName :: String,
    varGlobal :: Bool,
    varVirtualRegister :: Bool,
    varType :: Type
    }

data IROperand
    = IRVariable IRVariable
    | IRConstant IRConstant

data IRFunction = IRFunction {
    funName :: String,
    funRetType :: Type,
    funInstructions :: [IRInstruction],
    funParameters :: [IRVariable],
    funLocalVars :: [IRVariable],
    funVirtualRegs :: [IRVariable]
}

data IRProgram = IRProgram {
    progGlobalVars :: [IRVariable],
    progFunction :: [IRFunction]
}

newtype LABEL = LBL String

data IRInstruction
    = Jump Jump
    | Assignment Assignment
    | NOP
    | LABEL LABEL
    | STORE
    | RET

data Assignment
    = BinaryOperation BinaryOperation
    | CastOperation CastOperation
    | MOV
    | LOAD
    | CALL

data CastOperation
    = I2R
    | R2I

data BinaryOperation
    = ADD
    | SUB
    | MUL
    | DIV

data Jump 
    = ConditionalJump ConditionalJump
    | JMP

data ConditionalJump
    = JEQ
    | JNE
    | JLE
    | JLT
    | JGT
    | JGE
