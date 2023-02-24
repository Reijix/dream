module DumpIR (dumpIR) where

import Control.Monad
import IRSyntax
import System.IO

indentFunction :: String
indentFunction = "  "

indentLabel :: String
indentLabel = "    "

indentInstruction :: String
indentInstruction = "      "

dumpIR :: Handle -> IRProgram -> IO ()
dumpIR = visitProgram

visitInstruction :: Handle -> IRInstruction -> IO ()
visitInstruction handle (LABEL lbl) = hPutStrLn handle $ indentLabel ++ show lbl ++ ":"
visitInstruction handle instr = hPutStrLn handle $ indentInstruction ++ show instr

printVariableDeclarations :: Handle -> [IRVariable] -> String -> String -> IO ()
printVariableDeclarations handle vars indent key = do
  mapM_ printVariable vars
  where
    printVariable :: IRVariable -> IO ()
    printVariable (IRVar name _ _ varType) = hPutStrLn handle $ indent ++ key ++ " " ++ name ++ " : " ++ show varType

visitProgram :: Handle -> IRProgram -> IO ()
visitProgram handle (IRProgram gVars funs) = do
  printVariableDeclarations handle gVars "" ".global"
  unless (null gVars) (hPutStr handle "\n")
  mapM_ (visitFunction handle) funs

visitFunction :: Handle -> IRFunction -> IO ()
visitFunction handle (IRFunction name retType instructions params lVars virtualRegs) = do
  -- prolog
  hPutStrLn handle $ ".func " ++ name ++ " : " ++ show retType
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
    printParams (IRVar name _ _ vType) = hPutStrLn handle $ indentFunction ++ ".param " ++ name ++ " : " ++ show vType
