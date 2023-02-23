module Symbol where

import Syntax ( Declaration (FunctionDeclaration), Expression (Identifier), Block (Block), TypeName (ArrayTypeName), PrimitiveType (REAL, INT) )
import Text.Parsec.Pos ( SourcePos, newPos )

data Type 
    = VoidType
    | FunctionType Type [Type]          -- returnType, parameterTypes
    | ArrayType PrimitiveType [Int]     -- baseType,   lengths
    | PrimType PrimitiveType deriving (Ord, Eq)
instance Show Type where
    show VoidType = "void"
    show (FunctionType retType paramTypes) = foldr foldParams "" paramTypes ++ show retType
        where
            foldParams :: Type -> String -> String
            foldParams pType rest = show pType ++ " -> " ++ rest
    show (ArrayType baseType dimensions) = show (PrimType baseType) ++ foldl foldDims "" dimensions
        where
            foldDims :: String -> Int -> String
            foldDims rest dim = rest ++ "[" ++ show dim ++ "]" 
    show (PrimType INT) = "int"
    show (PrimType REAL) = "real"

-- TODO maybe function_scope should be removed and instead we use global_scope...
data Scope 
    = GLOBAL_SCOPE
    | PARAMETER_SCOPE
    | LOCAL_SCOPE
    | FUNCTION_SCOPE deriving (Show, Ord, Eq)

data Symbol = Symbol {
    symbolIdentifier :: String,         -- identifier
    symbolType :: Type,                 -- type
    symbolDeclaration :: Declaration,   -- node where this was declared
    symbolScope :: Scope                -- scope in which it was declared
    } deriving (Ord, Eq)
instance Show Symbol where
    show (Symbol ident sType decl scope) = "[Symbol '" ++ ident ++ "' '" ++ show sType ++ "' '" ++ show scope ++ "']"

-- dummy declaration for prelude definition
preludePos :: SourcePos
preludePos = newPos "Prelude" 0 0

dummyDeclaration :: Declaration
dummyDeclaration = FunctionDeclaration (Identifier "PreludeFunction" preludePos) [] Nothing (Block [] []) preludePos

int :: Type
int = PrimType INT
real :: Type
real = PrimType REAL

preludeSymbols :: [Symbol]
preludeSymbols = [
    Symbol "writeChar" (FunctionType int [int]) dummyDeclaration FUNCTION_SCOPE,
    Symbol "readChar" (FunctionType int []) dummyDeclaration FUNCTION_SCOPE,
    Symbol "writeInt" (FunctionType int [int]) dummyDeclaration FUNCTION_SCOPE,
    Symbol "readInt" (FunctionType int []) dummyDeclaration FUNCTION_SCOPE,
    Symbol "writeReal" (FunctionType int [real]) dummyDeclaration FUNCTION_SCOPE,
    Symbol "readReal" (FunctionType real []) dummyDeclaration FUNCTION_SCOPE,
    Symbol "exit" (FunctionType int [int]) dummyDeclaration FUNCTION_SCOPE
    ]