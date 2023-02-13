module NameAnalysis (doNameAnalysis) where
import SymbolTable
import Symbol
import Syntax
import Data.Set ( Set, empty, insert )

type DefinitionTable = Set String
type NAState = ([DefinitionTable], SymbolTable)

-- open new context and go over declarations
-- first go over global variables and then over functions
-- TODO prefill DefinitionTable with prelude definitions
doNameAnalysis :: Program -> NAState
doNameAnalysis (Program decls) = stateAfterFunctions
    where
        globalVariables = [var | var@(VariableDeclaration {}) <- decls]
        functionDeclarations = [fun | fun@(FunctionDeclaration {}) <- decls]
        initialState = ([empty], emptySymbolTable)
        stateAfterGlobalVariables = foldr visitGlobalVariable initialState globalVariables
        stateAfterFunctions = foldr visitFunctionDeclaration stateAfterGlobalVariables functionDeclarations

visitGlobalVariable :: Declaration -> NAState -> NAState
visitGlobalVariable decl@(VariableDeclaration (Identifier name) tName) (dt:dts, st) = (new_dt:dts, new_st)
    where
        -- TODO check if variable already defined
        new_dt = insert name dt
        new_st = insertDeclarationSymbol decl symbol st
        symbol = Symbol name TDummy decl GLOBAL_SCOPE 

visitFunctionDeclaration :: Declaration -> NAState -> NAState
-- TODO check if symbol already defined
visitFunctionDeclaration decl@(FunctionDeclaration (Identifier name) params retType block) (dt:dts, st) = (new_dts, new_st)
    where
        dt_with_fun = insert name dt
        inner_dts = empty:dt_with_fun:dts -- open new scope
        -- visit parameters
        inner_st = insertDeclarationSymbol decl symbol st
        symbol = Symbol name TDummy decl FUNCTION_SCOPE
        param_st = foldr visitParameterDeclaration (inner_dts, inner_st) params
        -- visit block
        (_:new_dts, new_st) = visitBlock block param_st -- close scope

-- TODO IMPLEMENT
visitParameterDeclaration :: Declaration -> NAState -> NAState
visitParameterDeclaration decl@(ParameterDeclaration (Identifier name) tName) (dt:dts, st) = (dt:dts, st)

-- TODO IMPLEMENT
visitBlock :: Block -> NAState -> NAState
visitBlock (Block decls stmnts) (dt:dts, st) = (dt:dts, st)

-- TODO IMPLEMENT
visitExpression :: Expression -> NAState -> NAState
visitExpression expr state = state