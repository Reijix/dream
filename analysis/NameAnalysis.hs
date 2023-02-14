module NameAnalysis (doNameAnalysis) where
import SymbolTable
import Symbol
import Syntax
import Data.Map ( Map, lookup, empty, insert, notMember )
import Prelude hiding ( lookup )
import Data.Maybe ( fromMaybe )
import Data.Foldable ( foldl' )

type DefinitionTable = Map String Symbol
type NAState = ([DefinitionTable], SymbolTable)

-- runner method for nameanalysis, only this needs to be exported
doNameAnalysis :: Program -> SymbolTable
doNameAnalysis prog = st
    where
        (_, st) = visitProgram ([preludeDefinitions], emptySymbolTable) prog
        preludeDefinitions = foldl' insertSymb empty preludeSymbols
        insertSymb :: DefinitionTable -> Symbol -> DefinitionTable
        insertSymb dt symb@(Symbol name _ _ _) = insert name symb dt

-- helper function for retrieving the symbol for a given identifier, throws error if symbol not defined
getSymbol :: String -> [DefinitionTable] -> Symbol
getSymbol name [] = error ("Symbol with name: " ++ name ++ " is not defined!")
getSymbol name (dt:dts) = fromMaybe (getSymbol name dts) (lookup name dt)

-- open new context and go over declarations
-- first go over global variables and then over functions
visitProgram :: NAState -> Program -> NAState
visitProgram state (Program decls) = stateAfterFunctions
    where
        globalVariables = [var | var@(VariableDeclaration {}) <- decls]
        functionDeclarations = [fun | fun@(FunctionDeclaration {}) <- decls]
        stateAfterGlobalVariables = foldl' visitGlobalVariable state globalVariables
        stateAfterFunctions = foldl' visitFunctionDeclaration stateAfterGlobalVariables functionDeclarations

visitGlobalVariable :: NAState -> Declaration -> NAState
visitGlobalVariable (dt:dts, st) decl@(VariableDeclaration (Identifier name) tName) = 
    if notMember name dt then (new_dt:dts, new_st)
    else error ("Error during nameanalysis, global variable " ++ name ++ ", multiple definitions!")
    where
        new_dt = insert name symbol dt
        new_st = insertDeclarationSymbol decl symbol st
        symbol = Symbol name TDummy decl GLOBAL_SCOPE

visitFunctionDeclaration :: NAState -> Declaration -> NAState
visitFunctionDeclaration (dt:dts, st) decl@(FunctionDeclaration (Identifier name) params retType block) = 
    if notMember name dt then (new_dt:dts, new_st)
    else error ("Error during nameanalysis, functiondeclaration " ++ name ++ ", already defined!")
    where
        new_dt = insert name symbol dt
        inner_dts = empty:new_dt:dts -- open new scope
        -- visit parameters
        inner_st = insertDeclarationSymbol decl symbol st
        symbol = Symbol name TDummy decl FUNCTION_SCOPE
        param_st = foldl' visitParameterDeclaration (inner_dts, inner_st) params
        -- visit block
        (_, new_st) = visitBlock param_st block
visitFunctionDeclaration (dts, st) decl = error $ "visitFunctionDeclaration was called with:\n" ++ show decl ++ "\nand dts:\n" ++ show dts

visitParameterDeclaration :: NAState -> Declaration -> NAState
visitParameterDeclaration (dt:dts, st) decl@(ParameterDeclaration (Identifier name) tName) = 
    if notMember name dt then (new_dt:dts, new_st)
    else error ("Error during nameanalysis, parameterdeclaration " ++ name ++ ", already defined!")
    where
        new_dt = insert name symbol dt
        new_st = insertDeclarationSymbol decl symbol st
        symbol = Symbol name TDummy decl PARAMETER_SCOPE

visitLocalVariable :: NAState -> Declaration -> NAState
visitLocalVariable (dt:dts, st) decl@(VariableDeclaration (Identifier name) tName) =
    if notMember name dt then (new_dt:dts, new_st)
    else error ("Error during nameanalysis, local variable " ++ name ++ ", already defined!")
    where
        new_dt = insert name symbol dt
        new_st = insertDeclarationSymbol decl symbol st
        symbol = Symbol name TDummy decl LOCAL_SCOPE

visitBlock :: NAState -> Block -> NAState
visitBlock (dts, st) (Block decls stmnts) = (dts, new_st)
    where
        -- visit declarations
        (inner_dts, inner_st) = foldl' visitLocalVariable (empty:dts, st) decls -- TODO optimization, we dont need to pass all of dts in here
        -- visit statements
        (_, new_st) = foldl' visitStatement (inner_dts, inner_st) stmnts


visitStatement :: NAState -> Statement -> NAState
visitStatement state (AssignStatement e1 e2) = visitExpression state e1 `visitExpression` e2
visitStatement state (FunctionCallStatement e1) = visitExpression state e1
visitStatement state (IfStatement e1 thenB (Just elseB)) = visitExpression state e1 `visitBlock` thenB `visitBlock` elseB
visitStatement state (IfStatement e1 thenB Nothing) = visitExpression state e1 `visitBlock` thenB
visitStatement state (WhileStatement e1 block) = visitExpression state e1 `visitBlock` block
visitStatement state (ReturnStatement (Just e)) = visitExpression state e
visitStatement state (ReturnStatement Nothing) = state

visitExpression :: NAState -> Expression -> NAState
visitExpression state (ArrayAccess expr exprs) = foldl' visitExpression (visitExpression state expr) exprs
visitExpression state (BinaryExpression e1 op e2) = visitExpression state e1 `visitExpression` e2
visitExpression state (Constant _) = state
visitExpression state (FunctionCall expr exprs) = foldl' visitExpression (visitExpression state expr) exprs
visitExpression (dts, st) expr@(Identifier name) = (dts, new_st)
    where
        symbol = getSymbol name dts -- throws error if symbol is not defined
        new_st = insertExpressionSymbol expr symbol st
visitExpression state (TypeCast e1 tName) = visitExpression state e1
