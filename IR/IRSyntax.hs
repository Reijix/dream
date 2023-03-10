module IRSyntax where

import Symbol (Type)

data IRConstant
  = IRIntConstant Int
  | IRRealConstant Double

data IRVariable = IRVar
  { varName :: String,
    varGlobal :: Bool,
    varVirtualRegister :: Bool,
    varType :: Type
  }
  deriving (Eq, Ord)

data IROperand
  = IRVariable IRVariable
  | IRConstant IRConstant

data IRFunction = IRFunction
  { funName :: String,
    funRetType :: Type,
    funInstructions :: [IRInstruction],
    funParameters :: [IRVariable],
    funLocalVars :: [IRVariable],
    funVirtualRegs :: [IRVariable]
  }

instance Eq IRFunction where
  fun1 == fun2 = funName fun1 == funName fun2

instance Ord IRFunction where
  fun1 <= fun2 = funName fun1 <= funName fun2

data IRProgram = IRProgram
  { progGlobalVars :: [IRVariable],
    progFunction :: [IRFunction]
  }

newtype LABEL = LBL Int

data IRInstruction
  = Jump LABEL Jump
  | Assignment Assignment
  | NOP
  | LABEL LABEL
  | STORE IRVariable IROperand IROperand
  | RET (Maybe IROperand)

data Assignment
  = BinaryOperation IRVariable IROperand IROperand BinaryOperation
  | CastOperation IRVariable IROperand Type Type CastOperation
  | MOV IRVariable IROperand
  | LOAD IRVariable IRVariable IROperand
  | CALL (Maybe IRVariable) String Type [IROperand]

data CastOperation
  = I2R
  | R2I

data BinaryOperation
  = ADD
  | SUB
  | MUL
  | DIV

data Jump
  = ConditionalJump IROperand IROperand ConditionalJump
  | JMP

data ConditionalJump
  = JEQ
  | JNE
  | JLE
  | JLT
  | JGT
  | JGE
