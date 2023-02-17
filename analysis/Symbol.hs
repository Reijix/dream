module Symbol where

import Syntax ( Declaration (FunctionDeclaration), Expression (Identifier), Block (Block), TypeName (ArrayTypeName), PrimitiveType )
import Text.Parsec.Pos ( SourcePos, newPos )

data Type 
    = VoidType
    | FunctionType Type [Type]          -- returnType, parameterTypes
    | ArrayType PrimitiveType [Int]     -- baseType,   lengths
    | PrimType PrimitiveType deriving (Show, Eq)

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
    show (Symbol ident sType _ _) = "[Symbol '" ++ ident ++ "' '" ++ show sType ++ "']"

-- dummy declaration for prelude definition
preludePos :: SourcePos
preludePos = newPos "Prelude" 0 0

dummyDeclaration :: Declaration
dummyDeclaration = FunctionDeclaration (Identifier "PreludeFunction") [] Nothing (Block [] []) preludePos

-- TODO add type information, when implementing typeanalysis
preludeSymbols :: [Symbol]
preludeSymbols = [
    Symbol "writeChar" VoidType dummyDeclaration FUNCTION_SCOPE,
    Symbol "readChar" VoidType dummyDeclaration FUNCTION_SCOPE,
    Symbol "writeInt" VoidType dummyDeclaration FUNCTION_SCOPE,
    Symbol "readInt" VoidType dummyDeclaration FUNCTION_SCOPE,
    Symbol "writeReal" VoidType dummyDeclaration FUNCTION_SCOPE,
    Symbol "readReal" VoidType dummyDeclaration FUNCTION_SCOPE,
    Symbol "exit" VoidType dummyDeclaration FUNCTION_SCOPE
    ]