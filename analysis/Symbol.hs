module Symbol where

import Syntax ( Declaration )

-- TODO move this to typeanalysis later
data Type = TDummy

data Scope 
    = GLOBAL_SCOPE
    | PARAMETER_SCOPE
    | LOCAL_SCOPE
    | FUNCTION_SCOPE

data Symbol = Symbol {
    symbolIdentifier :: String,         -- identifier
    symbolType :: Type,                 -- type
    symbolDeclaration :: Declaration,   -- node where this was declared
    symbolScope :: Scope                -- scope in which it was declared
    }