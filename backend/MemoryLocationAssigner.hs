module MemoryLocationAssigner where

import IRSyntax
import Data.Map
import MemoryLocation
import Control.Monad.State
import ArchX86 (sizeOfType, sizeOfAddress)

data MLState = MLState {
    memoryLocations :: Map IRVariable MemoryLocation,
    stackFrameSizes :: Map IRFunction Int,
    offset :: Int
}

type MLMonad = State MLState

assignMemoryLocations :: IRProgram -> (Map IRVariable MemoryLocation, Map IRFunction Int)
assignMemoryLocations prog = (ml,sfs)
    where 
        MLState ml sfs _ = execState (visitProgram prog) initialState
        initialState = MLState empty empty 0

visitProgram :: IRProgram -> MLMonad ()
visitProgram  (IRProgram gVars functions) = do
    mapM_ assignGlobalVariable gVars
    mapM_ assignFunction functions
    return undefined

assignGlobalVariable :: IRVariable -> MLMonad ()
assignGlobalVariable var@(IRVar name True False _) = do
    ml <- gets memoryLocations
    modify (\state -> state {memoryLocations = insert var (StaticLocation name) ml})

assignFunction :: IRFunction -> MLMonad ()
assignFunction fun@(IRFunction _ _ _ params localVars virtualRegs) = do
    -- visit local variables
    mapM_ assignLocalVariable localVars
    -- visit virtual registers (uses same method as local variables)
    mapM_ assignLocalVariable virtualRegs
    sfs <- gets stackFrameSizes
    off <- gets offset
    modify (\state -> state {stackFrameSizes = insert fun (-off) sfs, offset = sizeOfAddress * 2})
    -- visit parameters
    mapM_ assignParameter params

assignLocalVariable :: IRVariable -> MLMonad ()
assignLocalVariable var@(IRVar name False _ varType) = do
    -- calculate offset
    off <- gets offset
    let newOff = off - sizeOfType varType
    -- get ml
    ml <- gets memoryLocations
    -- modify state
    modify (\state -> state {offset = newOff, memoryLocations = insert var (StackFrameLocation newOff) ml})

assignParameter :: IRVariable -> MLMonad ()
assignParameter var@(IRVar name False _ varType) = do
    off <- gets offset
    ml <- gets memoryLocations
    -- insert memorylocations and update offset
    modify (\state -> state {memoryLocations = insert var (StackFrameLocation off) ml, offset = off + sizeOfType varType})