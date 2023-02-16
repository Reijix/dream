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
getSymbol :: String -> [DefinitionTable] -> Maybe Symbol
getSymbol name [] = Nothing
getSymbol name (dt:dts) = 
    case mSymbol of
        Nothing -> getSymbol name dts
        Just symbol -> Just symbol
    where
        mSymbol = lookup name dt

-- open new context and go over declarations
-- first go over global variables and then over functions
visitProgram :: NAState -> Program -> NAState
visitProgram state (Program decls) = stateAfterFunctions
    where
        globalVariables = [var | var@(VariableDeclaration {}) <- decls]
        functionDeclarations = [fun | fun@(FunctionDeclaration {}) <- decls]
        stateAfterGlobalVariables = foldl' visitGlobalVariable state globalVariables
        stateAfterFunCollection = foldl' collectFunctionSymbols stateAfterGlobalVariables functionDeclarations
        stateAfterFunctions = foldl' visitFunctionDeclaration stateAfterFunCollection functionDeclarations

collectFunctionSymbols :: NAState -> Declaration -> NAState
collectFunctionSymbols (dt:dts, st) decl@(FunctionDeclaration (Identifier name) params retType block sourcePos) =
    if notMember name dt then (new_dt:dts, new_st)
    else error ("Error during nameanalysis, functiondeclaration " ++ name ++ " already defined!\nat: [" ++ show sourcePos ++ "]")
    where
        symbol = Symbol name TDummy decl FUNCTION_SCOPE
        new_dt = insert name symbol dt
        new_st = insertDeclarationSymbol decl symbol st

visitGlobalVariable :: NAState -> Declaration -> NAState
visitGlobalVariable (dt:dts, st) decl@(VariableDeclaration (Identifier name) tName sourcePos) = 
    if notMember name dt then (new_dt:dts, new_st)
    else error ("Error during nameanalysis, global variable " ++ name ++ " multiple definitions!\nat: [" ++ show sourcePos ++ "]")
    where
        new_dt = insert name symbol dt
        new_st = insertDeclarationSymbol decl symbol st
        symbol = Symbol name TDummy decl GLOBAL_SCOPE

visitFunctionDeclaration :: NAState -> Declaration -> NAState
visitFunctionDeclaration (dts, st) decl@(FunctionDeclaration (Identifier name) params retType block sourcePos) = (dts, new_st)
    where
        inner_dts = empty:dts -- open new scope
        -- visit parameters
        param_state = foldl' visitParameterDeclaration (inner_dts, st) params
        -- visit block
        (_, new_st) = visitBlock param_state block

visitParameterDeclaration :: NAState -> Declaration -> NAState
visitParameterDeclaration (dt:dts, st) decl@(ParameterDeclaration (Identifier name) tName sourcePos) = 
    if notMember name dt then (new_dt:dts, new_st)
    else error ("Error during nameanalysis, parameterdeclaration " ++ name ++ " already defined!\nat: [" ++ show sourcePos ++ "]")
    where
        new_dt = insert name symbol dt
        new_st = insertDeclarationSymbol decl symbol st
        symbol = Symbol name TDummy decl PARAMETER_SCOPE

visitLocalVariable :: NAState -> Declaration -> NAState
visitLocalVariable (dt:dts, st) decl@(VariableDeclaration (Identifier name) tName sourcePos) =
    if notMember name dt then (new_dt:dts, new_st)
    else error ("Error during nameanalysis, local variable " ++ name ++ " already defined!\nat: [" ++ show sourcePos ++ "]")
    where
        new_dt = insert name symbol dt
        new_st = insertDeclarationSymbol decl symbol st
        symbol = Symbol name TDummy decl LOCAL_SCOPE

visitBlock :: NAState -> Block -> NAState
visitBlock (dts, st) (Block decls stmnts) = (dts, new_st)
    where
        -- visit declarations
        (inner_dts, inner_st) = foldl' visitLocalVariable (empty:dts, st) decls
        -- visit statements
        (_, new_st) = foldl' visitStatement (inner_dts, inner_st) stmnts


visitStatement :: NAState -> Statement -> NAState
visitStatement state (AssignStatement e1 e2 sourcePos) = visitExpression state e1 `visitExpression` e2
visitStatement state (FunctionCallStatement e1 sourcePos) = visitExpression state e1
visitStatement state (IfStatement e1 thenB (Just elseB) sourcePos) = visitExpression state e1 `visitBlock` thenB `visitBlock` elseB
visitStatement state (IfStatement e1 thenB Nothing sourcePos) = visitExpression state e1 `visitBlock` thenB
visitStatement state (WhileStatement e1 block sourcePos) = visitExpression state e1 `visitBlock` block
visitStatement state (ReturnStatement (Just e) sourcePos) = visitExpression state e
visitStatement state (ReturnStatement Nothing sourcePos) = state

visitExpression :: NAState -> Expression -> NAState
visitExpression state (ArrayAccess expr exprs sourcePos) = foldl' visitExpression (visitExpression state expr) exprs
visitExpression state (BinaryExpression e1 op e2 sourcePos) = visitExpression state e1 `visitExpression` e2
visitExpression state (Constant _ sourcePos) = state
visitExpression state (FunctionCall expr exprs sourcePos) = foldl' visitExpression (visitExpression state expr) exprs
visitExpression (dts, st) expr@(Identifier name) = 
    case symbol of
        Nothing -> error $ "Identifier is used but not defined: " ++ name ++ "\ndts is:\n" ++ show dts
        Just symbol -> (dts, insertExpressionSymbol expr symbol st)
    where
        symbol = getSymbol name dts -- throws error if symbol is not defined
visitExpression state (TypeCast e1 tName sourcePos) = visitExpression state e1
