module CodeGenerator (generateCode) where

import ArchX86
import qualified Control.Monad
import Control.Monad.State
import Data.Map
import IRSyntax
import MemoryLocation
import MemoryLocationAssigner
import System.IO

data CGState = CGState
  { memoryLocations :: Map IRVariable MemoryLocation,
    stackFrameSizes :: Map IRFunction Int,
    handle :: Handle,
    currentSFS :: Int,
    comments :: Bool
  }

type CGMonad = StateT CGState IO

-- runner function
generateCode :: Handle -> IRProgram -> IO ()
generateCode handle prog = do
  Control.Monad.void (runStateT (visitProgram prog) (CGState ml sfs handle 0 True))
  where
    (ml, sfs) = assignMemoryLocations prog

-- helper functions
instance Show MemoryLocation where
  show (StackFrameLocation off) = show off ++ "(%rbp)"
  show (StaticLocation name) = name ++ "(%rip)"
  show (HardwareRegister name size) = "%" ++ name

locationForOperand :: IROperand -> CGMonad String
locationForOperand (IRVariable var) = do
  ml <- gets memoryLocations
  return . show $ ml ! var
locationForOperand op@(IRConstant constant) = return $ show op

locationForArrayAccess :: IRVariable -> String -> CGMonad String
locationForArrayAccess arrayVar index = do
  ml <- gets memoryLocations
  let arrayLocation = ml ! arrayVar
  return $ case arrayLocation of
    StaticLocation name -> name ++ "(, " ++ index ++ ", 8)"
    StackFrameLocation off -> show off ++ "(%rbp, " ++ index ++ ", 8)"
    HardwareRegister _ _ -> error "Somehow an array variable got assigned to a hardware register... MemoryLocationAssigner is broken!"

writeUnary :: String -> String -> String -> CGMonad ()
writeUnary mnemo op comment = do
  comments <- gets comments
  if comments
    then writeLine $ mnemo ++ " " ++ op ++ "    #" ++ comment
    else writeLine $ mnemo ++ " " ++ op

writeBinary :: String -> String -> String -> String -> CGMonad ()
writeBinary mnemo op1 op2 comment = do
  comments <- gets comments
  if comments
    then writeLine $ mnemo ++ " " ++ op1 ++ ", " ++ op2 ++ "    #" ++ comment
    else writeLine $ mnemo ++ " " ++ op1 ++ ", " ++ op2

writeLine :: String -> CGMonad ()
writeLine str = do
  handle <- gets handle
  liftIO $ hPutStrLn handle str

visitProgram :: IRProgram -> CGMonad ()
visitProgram (IRProgram gVars funs) = do
  mapM_ visitGlobalVariable gVars
  mapM_ visitFunction funs

visitGlobalVariable :: IRVariable -> CGMonad ()
visitGlobalVariable (IRVar name True False vType) = do
  writeBinary ".lcomm" name (show $ sizeOfType vType) "global variable declaration"

visitFunction :: IRFunction -> CGMonad ()
visitFunction fun@(IRFunction name retType instructions _ _ _) = do
  sfs <- gets stackFrameSizes
  let sfSize = sfs ! fun
  -- prolog
  writeUnary ".global" name ""
  writeLine $ name ++ ":"
  writeUnary "pushq" "%rbp" "save rbp"
  writeBinary "movq" "%rsp" "%rbp" "update rbp"

  -- reserve space for stackframe
  when (sfSize /= 0) $ writeBinary "subq" ('$' : show sfSize) "%rsp" "reserve space for local vars"

  -- set currentSFS for return instructions
  modify (\state -> state {currentSFS = sfSize})
  -- visit instruction
  mapM_ visitInstruction instructions

visitInstruction :: IRInstruction -> CGMonad ()
visitInstruction NOP = writeLine "nop"
visitInstruction label@(LABEL {}) = writeLine $ show label
visitInstruction (RET opM) = do
  -- handle return value
  case opM of
    Nothing -> return ()
    Just op -> do
      loc <- locationForOperand op
      let retLoc = show returnLocation
      writeBinary "movq" loc retLoc "return value [RET]"
  -- adjust rsp
  sfs <- gets currentSFS
  when (sfs /= 0) $ writeBinary "addq" ('$' : show sfs) "%rsp" "clean up stack-frame [RET]"
  -- epilogue
  writeUnary "popq" "%rbp" "restore rbp [RET]"
  writeLine "RET"
visitInstruction (Jump lbl JMP) = do
  writeUnary "jmp" (show lbl) "[JMP]"
visitInstruction (Jump lbl (ConditionalJump leftOp rightOp cndJmp)) = do
  leftStr <- locationForOperand leftOp
  rightStr <- locationForOperand rightOp
  let leftReg = show $ head scratchRegisters
  let rightReg = show . head $ tail scratchRegisters
  -- move leftOp into register
  writeBinary "movq" leftStr leftReg "leftOp to register [COND-JMP]"
  -- move rightOp into register
  writeBinary "movq" rightStr rightReg "rightOp to register [COND-JMP]"
  -- do compare
  writeBinary "cmpq" rightReg leftReg "do cmp [COND-JMP]"
  -- jump
  let jmp = case cndJmp of
        JEQ -> "je"
        JNE -> "jne"
        JLE -> "jle"
        JLT -> "jl"
        JGE -> "jge"
        JGT -> "jg"
  writeUnary jmp (show lbl) "[COND-JMP]"
visitInstruction (STORE target index op) = do
  let indexReg = show $ head scratchRegisters
  let opReg = show . head $ tail scratchRegisters
  indexStr <- locationForOperand index
  opStr <- locationForOperand op
  -- move index to rax and sign extend
  writeBinary "movq" indexStr indexReg "move index to register [STORE]"
  writeLine "cltq"

  -- get string for indirect addressing
  arrStr <- locationForArrayAccess target indexReg

  -- move operand to register
  writeBinary "movq" opStr opReg "move operand to register [STORE]"

  -- move register to desired pos
  writeBinary "movq" opReg arrStr "[STORE]"
visitInstruction (Assignment (MOV target source)) = do
  targetStr <- locationForOperand $ IRVariable target
  sourceStr <- locationForOperand source
  let reg = show $ head scratchRegisters
  -- move operand to register
  writeBinary "movq" sourceStr reg "source into register [MOV]"
  writeBinary "movq" reg targetStr "[MOV]"
visitInstruction (Assignment (LOAD target arr index)) = do
  targetStr <- locationForOperand $ IRVariable target
  indexStr <- locationForOperand index
  let indexReg = show $ head scratchRegisters
  let resultReg = show . head $ tail scratchRegisters
  -- move index into rax and sign extend
  writeBinary "movq" indexStr indexReg "move index to register [LOAD]"
  writeLine "cltq"

  -- string for indirect addressing
  addrStr <- locationForArrayAccess arr indexReg

  -- write result into register
  writeBinary "movq" addrStr resultReg "LOAD into register [LOAD]"
  -- then write into target
  writeBinary "movq" resultReg targetStr "move to target [LOAD]"
visitInstruction (Assignment (CALL retM name _ args)) = do
  -- push arguments to stack
  mapM_ handleArg (reverse args)
  -- call statement
  writeUnary "call" name "[CALL]"
  -- remove arguments from stack
  writeBinary "addq" ('$' : show (length args * sizeOfAddress)) "%rsp" "clear arguments from stack [CALL]"
  -- assign retVal
  case retM of
    Nothing -> return ()
    Just retVal -> do
      retStr <- locationForOperand $ IRVariable retVal
      let retLoc = show returnLocation
      writeBinary "movq" retLoc retStr "move return value [CALL]"
  return undefined
  where
    handleArg :: IROperand -> CGMonad ()
    handleArg op@(IRConstant constant) = do
      let scratch = show $ head scratchRegisters
      let argStr = show op
      writeBinary "movq" argStr scratch "immediate parameter to register [CALL]"
      writeUnary "pushq" scratch "argument to stack [CALL]"
    handleArg op = do
      let scratch = show $ head scratchRegisters
      argStr <- locationForOperand op
      writeUnary "pushq" argStr "argument to stack [CALL]"
visitInstruction (Assignment (BinaryOperation target lhs rhs DIV)) = do
  targetStr <- locationForOperand $ IRVariable target
  leftStr <- locationForOperand lhs
  rightStr <- locationForOperand rhs
  let regStr = show $ head scratchRegisters
  -- mov leftOp to rax (needs to be rax for div!) and sign extend
  writeBinary "movq" leftStr "%rax" "leftOp to %rax [DIV]"
  writeLine "cqto"
  -- mov rightOp to reg
  writeBinary "movq" rightStr regStr "rightOp to register [DIV]"
  -- div
  writeUnary "idivq" regStr "[DIV]"
  -- move result to target
  writeBinary "movq" "%rax" targetStr "move result to target [DIV]"
visitInstruction (Assignment (BinaryOperation target lhs rhs op)) = do
  targetStr <- locationForOperand $ IRVariable target
  leftStr <- locationForOperand lhs
  rightStr <- locationForOperand rhs
  let resultReg = show $ head scratchRegisters
  let opReg = show . head $ tail scratchRegisters
  -- leftOp to scratch
  writeBinary "movq" leftStr resultReg "move leftOp to scratch [BIN-OP]"
  -- rightOp to scratch
  writeBinary "movq" rightStr opReg "move rightOp to scratch [BIN-OP]"
  -- add
  writeBinary (show op) opReg resultReg "[BIN-OP]"
  -- move result to target
  writeBinary "movq" resultReg targetStr "move result to target [BIN-OP]"
