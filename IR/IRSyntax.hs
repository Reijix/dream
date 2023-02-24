module IRSyntax where

import Data.List (intercalate)
import Symbol (Type)

data IRConstant
  = IRIntConstant Int
  | IRRealConstant Double

instance Show IRConstant where
  show (IRIntConstant num) = show num
  show (IRRealConstant num) = show num

data IRVariable = IRVar
  { varName :: String,
    varGlobal :: Bool,
    varVirtualRegister :: Bool,
    varType :: Type
  }
  deriving (Eq, Ord)

instance Show IRVariable where
  show (IRVar name _ _ vType) = name

data IROperand
  = IRVariable IRVariable
  | IRConstant IRConstant

instance Show IROperand where
  show (IRVariable var) = show var
  show (IRConstant constant) = "$" ++ show constant

data IRFunction = IRFunction
  { funName :: String,
    funRetType :: Type,
    funInstructions :: [IRInstruction],
    funParameters :: [IRVariable],
    funLocalVars :: [IRVariable],
    funVirtualRegs :: [IRVariable]
  }
  deriving (Show)

instance Eq IRFunction where
  fun1 == fun2 = funName fun1 == funName fun2

instance Ord IRFunction where
  fun1 <= fun2 = funName fun1 <= funName fun2

data IRProgram = IRProgram
  { progGlobalVars :: [IRVariable],
    progFunction :: [IRFunction]
  }
  deriving (Show)

newtype LABEL = LBL Int

instance Show LABEL where
  show (LBL num) = "L" ++ show num

data IRInstruction
  = Jump LABEL Jump
  | Assignment Assignment
  | NOP
  | LABEL LABEL
  | STORE IRVariable IROperand IROperand
  | RET (Maybe IROperand)

instance Show IRInstruction where
  show (Jump lbl jmp) = show jmp ++ " " ++ show lbl
  show (Assignment ass) = show ass
  show NOP = "NOP"
  show (LABEL lbl) = show lbl ++ ":"
  show (STORE target index op) = show target ++ "[" ++ show index ++ "]" ++ " := store " ++ show op
  show (RET Nothing) = "ret"
  show (RET (Just op)) = "ret " ++ show op

data Assignment
  = BinaryOperation IRVariable IROperand IROperand BinaryOperation
  | CastOperation IRVariable IROperand Type Type CastOperation
  | MOV IRVariable IROperand
  | LOAD IRVariable IRVariable IROperand
  | CALL (Maybe IRVariable) String Type [IROperand]

instance Show Assignment where
  show (BinaryOperation target lhs rhs op) = show target ++ " := " ++ show op ++ " " ++ show lhs ++ ", " ++ show rhs
  show (CastOperation target op _ _ cast) = show target ++ " := " ++ show cast ++ " " ++ show op
  show (MOV target source) = show target ++ " := " ++ show source
  show (LOAD target from index) = show target ++ " := load " ++ show from ++ "[" ++ show index ++ "]"
  show (CALL Nothing name fType args) = name ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (CALL (Just target) name fType args) = show target ++ " := " ++ "call " ++ name ++ "(" ++ intercalate ", " (map show args) ++ ")"

data CastOperation
  = I2R
  | R2I

instance Show CastOperation where
  show I2R = "(I2R)"
  show R2I = "(R2I)"

data BinaryOperation
  = ADD
  | SUB
  | MUL
  | DIV

instance Show BinaryOperation where
  show ADD = "addq"
  show SUB = "subq"
  show MUL = "mulq"
  show DIV = "divq"

data Jump
  = ConditionalJump IROperand IROperand ConditionalJump
  | JMP

instance Show Jump where
  show (ConditionalJump lOp rOp cnd) = show cnd ++ " " ++ show lOp ++ ", " ++ show rOp ++ ","
  show JMP = "JMP"

data ConditionalJump
  = JEQ
  | JNE
  | JLE
  | JLT
  | JGT
  | JGE
  deriving (Show)
