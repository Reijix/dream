module Symbol where

import Syntax ( Declaration (FunctionDeclaration), Expression (Identifier), Block (Block) )
import Text.Parsec.Pos ( SourcePos, newPos )

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
    }

instance Show Symbol where
    show (Symbol ident _ _ _) = "Symbol '" ++ ident ++ "'"

-- dummy declaration for prelude definition
preludePos :: SourcePos
preludePos = newPos "Prelude" 0 0

dummyDeclaration :: Declaration
dummyDeclaration = FunctionDeclaration (Identifier "PreludeFunction") [] Nothing (Block [] []) preludePos

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