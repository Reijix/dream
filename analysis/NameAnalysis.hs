module NameAnalysis (doNameAnalysis) where
import SymbolTable
import Symbol
import Syntax
import Data.Map ( Map, lookup, empty, insert, notMember )
import Prelude hiding ( lookup )
import Data.Maybe ( fromMaybe )

type DefinitionTable = Map String Symbol
type NAState = ([DefinitionTable], SymbolTable)

-- runner method for nameanalysis, only this needs to be exported
doNameAnalysis :: Program -> SymbolTable
doNameAnalysis prog = st
    where
        (_, st) = visitProgram prog ([preludeDefinitions], emptySymbolTable)
        preludeDefinitions = foldr insertSymb empty preludeSymbols
        insertSymb :: Symbol -> DefinitionTable -> DefinitionTable
        insertSymb symb@(Symbol name _ _ _) = insert name symb

-- helper function for retrieving the symbol for a given identifier, throws error if symbol not defined
getSymbol :: String -> [DefinitionTable] -> Symbol
getSymbol name [] = error ("Symbol with name: " ++ name ++ " is not defined!")
getSymbol name (dt:dts) = fromMaybe (getSymbol name dts) (lookup name dt)

-- open new context and go over declarations
-- first go over global variables and then over functions
visitProgram :: Program -> NAState -> NAState
visitProgram (Program decls) state = stateAfterFunctions
    where
        globalVariables = [var | var@(VariableDeclaration {}) <- decls]
        functionDeclarations = [fun | fun@(FunctionDeclaration {}) <- decls]
        stateAfterGlobalVariables = foldr visitGlobalVariable state globalVariables
        stateAfterFunctions = foldr visitFunctionDeclaration stateAfterGlobalVariables functionDeclarations

visitGlobalVariable :: Declaration -> NAState -> NAState
visitGlobalVariable decl@(VariableDeclaration (Identifier name) tName) (dt:dts, st) = 
    if notMember name dt then (new_dt:dts, new_st)
    else error ("Error during nameanalysis, global variable " ++ name ++ ", multiple definitions!")
    where
        new_dt = insert name symbol dt
        new_st = insertDeclarationSymbol decl symbol st
        symbol = Symbol name TDummy decl GLOBAL_SCOPE

visitFunctionDeclaration :: Declaration -> NAState -> NAState
visitFunctionDeclaration decl@(FunctionDeclaration (Identifier name) params retType block) (dt:dts, st) = 
    if notMember name dt then (new_dt:dts, new_st)
    else error ("Error during nameanalysis, functiondeclaration " ++ name ++ ", already defined!")
    where
        new_dt = insert name symbol dt
        inner_dts = empty:new_dt:dts -- open new scope
        -- visit parameters
        inner_st = insertDeclarationSymbol decl symbol st
        symbol = Symbol name TDummy decl FUNCTION_SCOPE
        param_st = foldr visitParameterDeclaration (inner_dts, inner_st) params
        -- visit block
        (_, new_st) = visitBlock block param_st
visitFunctionDeclaration decl (dts, st) = error $ "visitFunctionDeclaration was called with:\n" ++ show decl ++ "\nand dts:\n" ++ show dts

visitParameterDeclaration :: Declaration -> NAState -> NAState
visitParameterDeclaration decl@(ParameterDeclaration (Identifier name) tName) (dt:dts, st) = 
    if notMember name dt then (new_dt:dts, new_st)
    else error ("Error during nameanalysis, parameterdeclaration " ++ name ++ ", already defined!")
    where
        new_dt = insert name symbol dt
        new_st = insertDeclarationSymbol decl symbol st
        symbol = Symbol name TDummy decl PARAMETER_SCOPE

visitLocalVariable :: Declaration -> NAState -> NAState
visitLocalVariable decl@(VariableDeclaration (Identifier name) tName) (dt:dts, st) =
    if notMember name dt then (new_dt:dts, new_st)
    else error ("Error during nameanalysis, local variable " ++ name ++ ", already defined!")
    where
        new_dt = insert name symbol dt
        new_st = insertDeclarationSymbol decl symbol st
        symbol = Symbol name TDummy decl LOCAL_SCOPE

visitBlock :: Block -> NAState -> NAState
visitBlock (Block decls stmnts) (dts, st) = (dts, new_st)
    where
        -- visit declarations
        (inner_dts, inner_st) = foldr visitLocalVariable (empty:dts, st) decls -- TODO optimization, we dont need to pass all of dts in here
        -- visit statements
        (_, new_st) = foldr visitStatement (inner_dts, inner_st) stmnts


visitStatement :: Statement -> NAState -> NAState
visitStatement (AssignStatement e1 e2) state = visitExpression e2 $ visitExpression e1 state
visitStatement (FunctionCallStatement e1) state = visitExpression e1 state
visitStatement (IfStatement e1 thenB (Just elseB)) state = visitBlock elseB $ visitBlock thenB $ visitExpression e1 state
visitStatement (IfStatement e1 thenB Nothing) state = visitBlock thenB $ visitExpression e1 state
visitStatement (WhileStatement e1 block) state = visitBlock block $ visitExpression e1 state
visitStatement (ReturnStatement (Just e)) state = visitExpression e state
visitStatement (ReturnStatement Nothing) state = state

visitExpression :: Expression -> NAState -> NAState
visitExpression (ArrayAccess expr exprs) state = foldr visitExpression (visitExpression expr state) exprs
visitExpression (BinaryExpression e1 op e2) state = visitExpression e2 $ visitExpression e1 state
visitExpression (Constant _) state = state
visitExpression (FunctionCall expr exprs) state = foldr visitExpression (visitExpression expr state) exprs
visitExpression expr@(Identifier name) (dts, st) = (dts, new_st)
    where
        symbol = getSymbol name dts -- throws error if symbol is not defined
        new_st = insertExpressionSymbol expr symbol st
visitExpression (TypeCast e1 tName) state = visitExpression e1 state
