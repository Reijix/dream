module NameAnalysis (doNameAnalysis) where
import SymbolTable
import Symbol
import Syntax
import Data.Set ( Set, empty, insert, notMember, member )
import Data.Maybe ( fromMaybe )

type DefinitionTable = Set String
type NAState = ([DefinitionTable], SymbolTable)

-- TODO for each expression node we need to make a new entry in the symbolTable!!!
-- right now expression nodes are not assigned to symbols!

doNameAnalysis :: Program -> SymbolTable
doNameAnalysis prog = st
    where
        (_, st) = visitProgram prog ([empty], emptySymbolTable)

-- helper function for checking if a string is contained in a list of definitiontables
isDefined :: String -> [DefinitionTable] -> Bool
isDefined name (dt:dts) = member name dt || isDefined name dts 

-- open new context and go over declarations
-- first go over global variables and then over functions
-- TODO prefill DefinitionTable with prelude definitions
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
        new_dt = insert name dt
        new_st = insertDeclarationSymbol decl symbol st
        symbol = Symbol name TDummy decl GLOBAL_SCOPE

visitFunctionDeclaration :: Declaration -> NAState -> NAState
visitFunctionDeclaration decl@(FunctionDeclaration (Identifier name) params retType block) (dt:dts, st) = 
    if notMember name dt then (dts, new_st)
    else error ("Error during nameanalysis, functiondeclaration " ++ name ++ ", already defined!")
    where
        dt_with_fun = insert name dt
        inner_dts = empty:dt_with_fun:dts -- open new scope
        -- visit parameters
        inner_st = insertDeclarationSymbol decl symbol st
        symbol = Symbol name TDummy decl FUNCTION_SCOPE
        param_st = foldr visitParameterDeclaration (inner_dts, inner_st) params
        -- visit block
        (_, new_st) = visitBlock block param_st

visitParameterDeclaration :: Declaration -> NAState -> NAState
visitParameterDeclaration decl@(ParameterDeclaration (Identifier name) tName) (dt:dts, st) = 
    if notMember name dt then (new_dt:dts, new_st)
    else error ("Error during nameanalysis, parameterdeclaration " ++ name ++ ", already defined!")
    where
        new_dt = insert name dt
        new_st = insertDeclarationSymbol decl symbol st
        symbol = Symbol name TDummy decl PARAMETER_SCOPE

visitLocalVariable :: Declaration -> NAState -> NAState
visitLocalVariable decl@(VariableDeclaration (Identifier name) tName) (dt:dts, st) =
    if notMember name dt then (new_dt:dts, new_st)
    else error ("Error during nameanalysis, local variable " ++ name ++ ", already defined!")
    where
        new_dt = insert name dt
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
visitExpression (Identifier name) state@(dts, st) = if isDefined name dts then state else error ("Error during nameanalysis, identifier in expression " ++ name ++ " not defined!\nSymboltable is:\n" ++ show st)
visitExpression (TypeCast e1 tName) state = visitExpression e1 state
