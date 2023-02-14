module Symbol where

import Syntax ( Declaration (FunctionDeclaration), Expression (Identifier), Block (Block) )

-- TODO move this to typeanalysis later
data Type = TDummy deriving (Show)

-- TODO maybe function_scope should be removed and instead we use global_scope...
data Scope 
    = GLOBAL_SCOPE
    | PARAMETER_SCOPE
    | LOCAL_SCOPE
    | FUNCTION_SCOPE deriving (Show)

data Symbol = Symbol {
    symbolIdentifier :: String,         -- identifier
    symbolType :: Type,                 -- type
    symbolDeclaration :: Declaration,   -- node where this was declared
    symbolScope :: Scope                -- scope in which it was declared
    } deriving (Show)


-- dummy declaration for prelude definition
dummyDeclaration :: Declaration
dummyDeclaration = FunctionDeclaration (Identifier "PreludeFunction") [] Nothing (Block [] [])

-- TODO add type information, when implementing typeanalysis
preludeSymbols :: [Symbol]
preludeSymbols = [
    Symbol "writeChar" TDummy dummyDeclaration FUNCTION_SCOPE,
    Symbol "readChar" TDummy dummyDeclaration FUNCTION_SCOPE,
    Symbol "writeInt" TDummy dummyDeclaration FUNCTION_SCOPE,
    Symbol "readInt" TDummy dummyDeclaration FUNCTION_SCOPE,
    Symbol "writeReal" TDummy dummyDeclaration FUNCTION_SCOPE,
    Symbol "readReal" TDummy dummyDeclaration FUNCTION_SCOPE,
    Symbol "exit" TDummy dummyDeclaration FUNCTION_SCOPE
    ]