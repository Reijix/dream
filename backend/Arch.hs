module Arch where
import MemoryLocation (MemoryLocation (HardwareRegister))
import Symbol ( Type )

data Arch = Arch {
    registers :: [MemoryLocation],
    parameterRegisters :: [MemoryLocation],
    returnLocation :: MemoryLocation,
    calleeSaveRegisters :: [MemoryLocation],
    callerSaveRegister :: [MemoryLocation],
    sizeOfType :: Type -> Int,
    sizeOfAddress :: Int 
    }
