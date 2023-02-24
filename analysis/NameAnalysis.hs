module NameAnalysis (doNameAnalysis) where

import AnalysisError
import Control.Monad (foldM)
import Data.Either.Extra (maybeToEither)
import Data.Foldable (foldl')
import Data.Map (Map, empty, insert, lookup, notMember)
import Data.Maybe (fromMaybe)
import Debug.Trace
import Symbol
import SymbolTable
import Syntax
import Prelude hiding (lookup)

type DefinitionTable = Map String Symbol

type NAState = ([DefinitionTable], SymbolTable)

-- runner method for nameanalysis, only this needs to be exported
doNameAnalysis :: Program -> Either AnalysisError SymbolTable
doNameAnalysis prog = do
  (_, st) <- visitProgram ([preludeDefinitions], preludeSt) prog
  return st
  where
    preludeDefinitions = foldl' insertSymb empty preludeSymbols
    preludeSt = foldl' (\st symb@(Symbol name _ decl _) -> insertDeclarationSymbol decl symb st) emptySymbolTable preludeSymbols
    insertSymb :: DefinitionTable -> Symbol -> DefinitionTable
    insertSymb dt symb@(Symbol name _ _ _) = insert name symb dt

-- helper function for retrieving the symbol for a given identifier, throws error if symbol not defined
getSymbol :: String -> [DefinitionTable] -> Maybe Symbol
getSymbol name [] = Nothing
getSymbol name (dt : dts) =
  case mSymbol of
    Nothing -> getSymbol name dts
    Just symbol -> Just symbol
  where
    mSymbol = lookup name dt

-- open new context and go over declarations
-- first go over global variables and then over functions
visitProgram :: NAState -> Program -> Either AnalysisError NAState
visitProgram st (Program decls) = do
  st1@(dst1, inner_st1) <- foldM visitGlobalVariable st globalVariables
  st2@(dst2, inner_st2) <- foldM collectFunctionSymbols st1 functionDeclarations
  foldM visitFunctionDeclaration st2 functionDeclarations
  where
    globalVariables = [var | var@(VariableDeclaration {}) <- decls]
    functionDeclarations = [fun | fun@(FunctionDeclaration {}) <- decls]

collectFunctionSymbols :: NAState -> Declaration -> Either AnalysisError NAState
collectFunctionSymbols (dt : dts, st) decl@(FunctionDeclaration (Identifier name iSourcePos) params retType block sourcePos) = do
  let symbol = Symbol name VoidType decl FUNCTION_SCOPE
  let new_dt = insert name symbol dt
  let new_st = insertDeclarationSymbol decl symbol st

  if notMember name dt
    then return (new_dt : dts, new_st)
    else Left $ NameError sourcePos ("Functiondeclaration " ++ name ++ " already defined!")

visitGlobalVariable :: NAState -> Declaration -> Either AnalysisError NAState
visitGlobalVariable (dt : dts, st) decl@(VariableDeclaration (Identifier name iSourcePos) tName sourcePos) = do
  let symbol = Symbol name VoidType decl GLOBAL_SCOPE
  let new_dt = insert name symbol dt
  let new_st = insertDeclarationSymbol decl symbol st
  if notMember name dt
    then return (new_dt : dts, new_st)
    else Left $ NameError sourcePos ("Global variable " ++ name ++ " multiple definitions!")

visitFunctionDeclaration :: NAState -> Declaration -> Either AnalysisError NAState
visitFunctionDeclaration (dts, st) decl@(FunctionDeclaration (Identifier name iSourcePos) params retType block sourcePos) = do
  let inner_dts = empty : dts -- open new scope
  -- visit parameters
  param_state <- foldM visitParameterDeclaration (inner_dts, st) params
  -- visit block
  (_, new_st) <- visitBlock param_state block
  return (dts, new_st)

visitParameterDeclaration :: NAState -> Declaration -> Either AnalysisError NAState
visitParameterDeclaration (dt : dts, st) decl@(ParameterDeclaration (Identifier name iSourcePos) tName sourcePos) = do
  let symbol = Symbol name VoidType decl PARAMETER_SCOPE
  let new_dt = insert name symbol dt
  let new_st = insertDeclarationSymbol decl symbol st
  if notMember name dt
    then return (new_dt : dts, new_st)
    else Left $ NameError sourcePos ("Parameterdeclaration " ++ name ++ " already defined!")

visitLocalVariable :: NAState -> Declaration -> Either AnalysisError NAState
visitLocalVariable (dt : dts, st) decl@(VariableDeclaration (Identifier name iSourcePos) tName sourcePos) = do
  let symbol = Symbol name VoidType decl LOCAL_SCOPE
  let new_dt = insert name symbol dt
  let new_st = insertDeclarationSymbol decl symbol st
  if notMember name dt
    then return (new_dt : dts, new_st)
    else Left $ NameError sourcePos ("Local variable " ++ name ++ " already defined!")

visitBlock :: NAState -> Block -> Either AnalysisError NAState
visitBlock (dts, st) (Block decls stmnts) = do
  -- visit declarations
  (inner_dts, inner_st) <- foldM visitLocalVariable (empty : dts, st) decls
  -- visit statements
  (_, new_st) <- foldM visitStatement (inner_dts, inner_st) stmnts
  return (dts, new_st)

visitStatement :: NAState -> Statement -> Either AnalysisError NAState
visitStatement st (AssignStatement e1 e2 sourcePos) = do
  st1 <- visitExpression st e1
  visitExpression st1 e2
visitStatement st (FunctionCallStatement e1 sourcePos) = visitExpression st e1
visitStatement st (IfStatement e1 thenB (Just elseB) sourcePos) = do
  st1 <- visitExpression st e1
  st2 <- visitBlock st1 thenB
  visitBlock st2 elseB
visitStatement st (IfStatement e1 thenB Nothing sourcePos) = do
  st1 <- visitExpression st e1
  visitBlock st1 thenB
visitStatement st (WhileStatement e1 block sourcePos) = do
  st1 <- visitExpression st e1
  visitBlock st1 block
visitStatement st (ReturnStatement (Just e) sourcePos) = visitExpression st e
visitStatement st (ReturnStatement Nothing sourcePos) = return st

visitExpression :: NAState -> Expression -> Either AnalysisError NAState
visitExpression st (ArrayAccess expr exprs sourcePos) = do
  st1 <- visitExpression st expr
  foldM visitExpression st1 exprs
visitExpression st (BinaryExpression e1 op e2 sourcePos) = do
  st1 <- visitExpression st e1
  visitExpression st1 e2
visitExpression st (Constant _ sourcePos) = return st
visitExpression st (FunctionCall expr exprs sourcePos) = do
  st1 <- visitExpression st expr
  foldM visitExpression st1 exprs
visitExpression (dts, st) expr@(Identifier name sourcePos) = do
  symbol <- maybeToEither (NameError sourcePos $ "Identifier " ++ name ++ " is used but not defined!") (getSymbol name dts)
  new_st <- maybeToEither (NameError sourcePos "identifier has not been declared before being used!") (insertExpressionSymbol expr symbol st)
  return (dts, new_st)
visitExpression st (TypeCast e1 tName sourcePos) = visitExpression st e1
