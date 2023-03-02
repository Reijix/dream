module MemoryLocationAssigner (assignMemoryLocations, scratchRegisters) where

import ArchX86
import Control.Monad.State
import Data.Map
import IRSyntax
import MemoryLocation
import Arch

-- architecture that is currently used
-- hardcoded right now, but maybe variable later!
architecture :: Arch
architecture = archX86

data MLState = MLState
  { memoryLocations :: Map IRVariable MemoryLocation,
    stackFrameSizes :: Map IRFunction Int,
    offset :: Int
  }

type MLMonad = State MLState

scratchRegisters :: [MemoryLocation]
scratchRegisters =
  [ HardwareRegister "r12" 8,
    HardwareRegister "r13" 8,
    HardwareRegister "r14" 8,
    HardwareRegister "r15" 8
  ]

assignMemoryLocations :: IRProgram -> (Map IRVariable MemoryLocation, Map IRFunction Int)
assignMemoryLocations prog = (ml, sfs)
  where
    MLState ml sfs _ = execState (visitProgram prog) initialState
    initialState = MLState empty empty 0

visitProgram :: IRProgram -> MLMonad ()
visitProgram (IRProgram gVars functions) = do
  mapM_ assignGlobalVariable gVars
  mapM_ assignFunction functions

assignGlobalVariable :: IRVariable -> MLMonad ()
assignGlobalVariable var@(IRVar name True False _) = do
  ml <- gets memoryLocations
  modify (\state -> state {memoryLocations = insert var (StaticLocation name) ml})
assignGlobalVariable _ = error "assignGlobalVariable falsely called"

assignFunction :: IRFunction -> MLMonad ()
assignFunction fun@(IRFunction _ _ _ params localVars virtualRegs) = do
  -- visit local variables
  mapM_ assignLocalVariable localVars
  -- visit virtual registers (uses same method as local variables)
  mapM_ assignLocalVariable $ reverse virtualRegs
  sfs <- gets stackFrameSizes
  off <- gets offset
  modify (\state -> state {stackFrameSizes = insert fun (-off) sfs, offset = sizeOfAddress architecture * 2})
  -- visit parameters
  mapM_ assignParameter params
  -- reset offset
  modify (\state -> state {offset = 0})

assignLocalVariable :: IRVariable -> MLMonad ()
assignLocalVariable var@(IRVar name False _ varType) = do
  -- calculate offset
  off <- gets offset
  let newOff = off - sizeOfType architecture varType
  -- get ml
  ml <- gets memoryLocations
  -- modify state
  modify (\state -> state {offset = newOff, memoryLocations = insert var (StackFrameLocation newOff) ml})
assignLocalVariable _ = error "assignLocalVariable invalid call"


assignParameter :: IRVariable -> MLMonad ()
assignParameter var@(IRVar name False _ varType) = do
  off <- gets offset
  ml <- gets memoryLocations
  -- insert memorylocations and update offset
  modify (\state -> state {memoryLocations = insert var (StackFrameLocation off) ml, offset = off + sizeOfType architecture varType})
assignParameter _ = error "assignParameter invalid call"
