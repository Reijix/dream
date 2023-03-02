module ArchX86 (archX86) where

import MemoryLocation (MemoryLocation (HardwareRegister))
import Symbol ( Type(ArrayType, PrimType) )
import qualified Arch as A

archX86 :: A.Arch
archX86 = A.Arch {
  A.registers = registers,
  A.parameterRegisters = parameterRegisters,
  A.returnLocation = returnLocation,
  A.calleeSaveRegisters = calleeSaveRegisters,
  A.callerSaveRegister = callerSaveRegister,
  A.sizeOfType = sizeOfType,
  A.sizeOfAddress = sizeOfAddress
}

registers :: [MemoryLocation]
registers =
  [ HardwareRegister "rax" 8,
    HardwareRegister "rbx" 8,
    HardwareRegister "rcx" 8,
    HardwareRegister "rdx" 8,
    HardwareRegister "rdi" 8,
    HardwareRegister "rsi" 8,
    HardwareRegister "r8" 8,
    HardwareRegister "r9" 8,
    HardwareRegister "r10" 8,
    HardwareRegister "r11" 8,
    HardwareRegister "r12" 8,
    HardwareRegister "r13" 8,
    HardwareRegister "r14" 8,
    HardwareRegister "r15" 8,
    HardwareRegister "rsp" 8,
    HardwareRegister "rbp" 8
  ]

parameterRegisters :: [MemoryLocation]
parameterRegisters =
  [ HardwareRegister "rcx" 8,
    HardwareRegister "rdx" 8,
    HardwareRegister "rdi" 8,
    HardwareRegister "rsi" 8,
    HardwareRegister "r8" 8,
    HardwareRegister "r9" 8
  ]

returnLocation :: MemoryLocation
returnLocation = HardwareRegister "rax" 8

calleeSaveRegisters :: [MemoryLocation]
calleeSaveRegisters =
  [ HardwareRegister "rbx" 8,
    HardwareRegister "r12" 8,
    HardwareRegister "r13" 8,
    HardwareRegister "r14" 8,
    HardwareRegister "r15" 8,
    HardwareRegister "rbp" 8
  ]

callerSaveRegister :: [MemoryLocation]
callerSaveRegister =
  [ HardwareRegister "rax" 8,
    HardwareRegister "rcx" 8,
    HardwareRegister "rdx" 8,
    HardwareRegister "rdi" 8,
    HardwareRegister "rsi" 8,
    HardwareRegister "r8" 8,
    HardwareRegister "r9" 8,
    HardwareRegister "r10" 8,
    HardwareRegister "r11" 8,
    HardwareRegister "rsp" 8
  ]

sizeOfType :: Type -> Int
sizeOfType (PrimType _) = 8
sizeOfType (ArrayType baseType dimensions) = sizeOfType (PrimType baseType) * product dimensions
sizeOfType _ = error "sizeOfType called on type without concrete size!"

sizeOfAddress :: Int
sizeOfAddress = 8
