module DumpIR (dumpIR) where

import Control.Monad
import IRSyntax
import System.IO
import Symbol
import Syntax (PrimitiveType (INT, REAL))
import Data.List (intercalate)

indentFunction :: String
indentFunction = "  "

indentLabel :: String
indentLabel = "    "

indentInstruction :: String
indentInstruction = "      "

class IRShow a where
  irShow :: a -> String

instance IRShow IRVariable where
  irShow (IRVar name _ _ _) = name
instance IRShow IROperand where
  irShow (IRVariable var) = irShow var
  irShow (IRConstant constant) = "$" ++ irShow constant
instance IRShow IRConstant where
  irShow (IRIntConstant num) = show num
  irShow (IRRealConstant num) = show num
instance IRShow LABEL where
  irShow (LBL num) = "L" ++ show num
instance IRShow IRInstruction where
  irShow (Jump lbl jmp) = irShow jmp ++ " " ++ irShow lbl
  irShow (Assignment ass) = irShow ass
  irShow NOP = "NOP"
  irShow (LABEL lbl) = irShow lbl ++ ":"
  irShow (STORE target index op) = irShow target ++ "[" ++ irShow index ++ "]" ++ " := store " ++ irShow op
  irShow (RET Nothing) = "ret"
  irShow (RET (Just op)) = "ret " ++ irShow op
instance IRShow Assignment where
  irShow (BinaryOperation target lhs rhs op) = irShow target ++ " := " ++ irShow op ++ " " ++ irShow lhs ++ ", " ++ irShow rhs
  irShow (CastOperation target op _ _ cast) = irShow target ++ " := " ++ irShow cast ++ " " ++ irShow op
  irShow (MOV target source) = irShow target ++ " := " ++ irShow source
  irShow (LOAD target from index) = irShow target ++ " := load " ++ irShow from ++ "[" ++ irShow index ++ "]"
  irShow (CALL Nothing name fType args) = name ++ "(" ++ intercalate ", " (map irShow args) ++ ")"
  irShow (CALL (Just target) name fType args) = irShow target ++ " := " ++ "call " ++ name ++ "(" ++ intercalate ", " (map irShow args) ++ ")"
instance IRShow CastOperation where
  irShow I2R = "(I2R)"
  irShow R2I = "(R2I)"
instance IRShow BinaryOperation where
  irShow ADD = "addq"
  irShow SUB = "subq"
  irShow MUL = "mulq"
  irShow DIV = "divq"
instance IRShow Jump where
  irShow (ConditionalJump lOp rOp cnd) = irShow cnd ++ " " ++ irShow lOp ++ ", " ++ irShow rOp ++ ","
  irShow JMP = "JMP"
instance IRShow ConditionalJump where
  irShow JEQ = "je"
  irShow JNE = "jne"
  irShow JLE = "jle"
  irShow JLT = "jlt"
  irShow JGT = "jgt"
  irShow JGE = "jge"
instance IRShow Type where
  irShow VoidType = "void"
  irShow (FunctionType retType paramTypes) = foldr foldParams "" paramTypes ++ irShow retType
    where
      foldParams :: Type -> String -> String
      foldParams pType rest = irShow pType ++ " -> " ++ rest
  irShow (ArrayType baseType dimensions) = irShow (PrimType baseType) ++ foldl foldDims "" dimensions
    where
      foldDims :: String -> Int -> String
      foldDims rest dim = rest ++ "[" ++ show dim ++ "]"
  irShow (PrimType INT) = "int"
  irShow (PrimType REAL) = "real"

dumpIR :: Handle -> IRProgram -> IO ()
dumpIR = visitProgram

visitInstruction :: Handle -> IRInstruction -> IO ()
visitInstruction handle (LABEL lbl) = hPutStrLn handle $ indentLabel ++ irShow lbl ++ ":"
visitInstruction handle instr = hPutStrLn handle $ indentInstruction ++ irShow instr

printVariableDeclarations :: Handle -> [IRVariable] -> String -> String -> IO ()
printVariableDeclarations handle vars indent key = do
  mapM_ printVariable vars
  where
    printVariable :: IRVariable -> IO ()
    printVariable (IRVar name _ _ varType) = hPutStrLn handle $ indent ++ key ++ " " ++ name ++ " : " ++ irShow varType

visitProgram :: Handle -> IRProgram -> IO ()
visitProgram handle (IRProgram gVars funs) = do
  printVariableDeclarations handle gVars "" ".global"
  unless (null gVars) (hPutStr handle "\n")
  mapM_ (visitFunction handle) funs

visitFunction :: Handle -> IRFunction -> IO ()
visitFunction handle (IRFunction name retType instructions params lVars virtualRegs) = do
  -- prolog
  hPutStrLn handle $ ".func " ++ name ++ " : " ++ irShow retType
  -- print parameters
  mapM_ printParams params
  -- print local variables
  printVariableDeclarations handle lVars indentFunction ".local"
  -- print virtual registers
  printVariableDeclarations handle (reverse virtualRegs) indentFunction ".virt"
  -- print code
  hPutStrLn handle $ indentFunction ++ ".code"
  mapM_ (visitInstruction handle) instructions
  where
    printParams :: IRVariable -> IO ()
    printParams (IRVar name _ _ vType) = hPutStrLn handle $ indentFunction ++ ".param " ++ name ++ " : " ++ irShow vType
