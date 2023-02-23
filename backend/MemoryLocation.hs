module MemoryLocation where

data MemoryLocation
    = StackFrameLocation Int
    | StaticLocation String
    | HardwareRegister String Int deriving (Show, Eq)