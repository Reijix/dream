module TypeAnalysis where

import SymbolTable
import AnalysisError
import Syntax
import Symbol
import Data.Foldable (Foldable(foldr'))
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)
import Data.Either.Extra

-- TODO maybe symboltable can be refactored, the indirection seems useless...


-- Does type analysis on an AST using the SymbolTable that was constructed during NameAnalysis
-- The type analysis returns a new SymbolTable containing Type information and a new AST containing casts where necessary
doTypeAnalysis :: SymbolTable -> Program -> Either AnalysisError (SymbolTable, Program)
doTypeAnalysis st prog = do
    (st, new_prog) <- visitProgram (st, prog)
    return (st, new_prog)


visitProgram :: (SymbolTable, Program) -> Either AnalysisError (SymbolTable, Program)
visitProgram (st, Program decls) = do
    ret@(new_st, Program new_decls) <- foldM f (st, Program []) (reverse decls)
    return ret
    where
        f :: (SymbolTable, Program) -> Declaration -> Either AnalysisError (SymbolTable, Program)
        f (st, Program decls) decl = do
            (new_st, new_decl) <- visitDeclaration (st, decl)
            return (new_st, Program (new_decl : decls))


visitDeclaration :: (SymbolTable, Declaration) -> Either AnalysisError (SymbolTable, Declaration)
visitDeclaration (st, decl@(FunctionDeclaration ident@(Identifier name) params retType block sourcePos)) = do
    -- check return type
    return_type <- get_return_type
    -- visit parameters
    (st1, new_params) <- foldM paramFoldFun (st, []) (reverse params)
    -- get parameterTypes
    let param_types = map extract_type params
    -- construct functionType
    let funtype = FunctionType return_type param_types
    -- visit block
    (st2, new_block) <- visitBlock (st1, block)
    -- construct new declaration node
    let new_decl = FunctionDeclaration ident new_params retType new_block sourcePos
    -- get old symbol
    (Symbol old_ident old_type old_decl old_scope ) <- maybeToEither (TypeError sourcePos "Symbol for function not found, how could this happen?!") (symbolForDeclaration decl st)
    -- construct new symbol
    let new_symbol = Symbol old_ident funtype new_decl old_scope
    -- add new symbol to symboltable
    let st3 = insertDeclarationSymbol new_decl new_symbol st2
    -- TODO remove old symbol...
    return (st3, new_decl)
    where
        extract_type :: Declaration -> Type
        extract_type decl = dType
            where
                (Just (Symbol _ dType _ _)) = symbolForDeclaration decl st
        paramFoldFun :: (SymbolTable, [Declaration]) -> Declaration -> Either AnalysisError (SymbolTable, [Declaration])
        paramFoldFun (st, decls) decl = do
            (st1, new_decl) <- visitDeclaration (st, decl)
            return (st1, new_decl : decls)
        get_return_type :: Either AnalysisError Type
        get_return_type = case retType of
            Nothing -> Right VoidType
            Just (PrimitiveTypeName pType) -> Right $ PrimType pType
            Just (ArrayTypeName {}) -> Left $ TypeError sourcePos "Function return type must be primitive!"
visitDeclaration (st, decl@(ParameterDeclaration ident@(Identifier name) paramType sourcePos)) = do
    -- get type
    param_type <- get_param_type paramType
    -- get old symbol
    (Symbol old_ident old_type old_decl old_scope ) <- maybeToEither (TypeError sourcePos "Symbol for function not found, how could this happen?!") (symbolForDeclaration decl st)
    -- construct new symbol
    let new_symbol = Symbol old_ident param_type decl old_scope
    -- update symboltable
    let st1 = insertDeclarationSymbol decl new_symbol st
    return (st1, decl)
    where
        get_param_type :: TypeName -> Either AnalysisError Type
        get_param_type paramType = do 
            case paramType of
                PrimitiveTypeName primType -> Right $ PrimType primType
                _ -> Left $ TypeError sourcePos "Function parameters must have primitive type!"
visitDeclaration (st, decl@(VariableDeclaration ident@(Identifier name) varType sourcePos)) = do
    -- get type
    (st1, var_type) <- get_var_type st varType
    -- get old symbol
    (Symbol old_ident old_type old_decl old_scope ) <- maybeToEither (TypeError sourcePos "Symbol for variable not found, how could this happen?!") (symbolForDeclaration decl st1)
    -- construct new symbol
    let new_symbol = Symbol old_ident var_type decl old_scope
    -- update symboltable
    let st2 = insertDeclarationSymbol decl new_symbol st1
    return (st2, decl)
    where
        get_var_type :: SymbolTable -> TypeName -> Either AnalysisError (SymbolTable, Type)
        get_var_type st varType = do 
            case varType of
                PrimitiveTypeName primType -> Right (st, PrimType primType)
                ArrayTypeName baseType lengths -> do
                    -- make sure length expressions are constants of type int
                    new_lengths <- foldM checkConstantInt [] (reverse lengths)
                    -- get baseType
                    base_type <- extractPrimitiveType baseType
                    return (st, ArrayType base_type new_lengths)
                    where
                        checkConstantInt :: [Int] -> Expression -> Either AnalysisError [Int]
                        checkConstantInt nums (Constant (IntLit num) _) = return $ num : nums
                        checkConstantInt _ _ = Left $ TypeError sourcePos "ArrayAccess expressions need to be constant integers!"
                        extractPrimitiveType :: TypeName -> Either AnalysisError PrimitiveType
                        extractPrimitiveType (PrimitiveTypeName INT) = Right INT
                        extractPrimitiveType _ = Left $ TypeError sourcePos "Arraylengths need to be of integer type!"

visitBlock :: (SymbolTable, Block) -> Either AnalysisError (SymbolTable, Block)
visitBlock (st, Block decls stmnts)= do
    -- visit declarations
    (st1, new_decls) <- foldM declFun (st, []) (reverse decls)
    -- visit statements
    (st2, new_stmnts) <- foldM stmntFun (st1, []) (reverse stmnts)
    return (st2, Block new_decls new_stmnts)
    where
        declFun :: (SymbolTable, [Declaration]) -> Declaration -> Either AnalysisError (SymbolTable, [Declaration])
        declFun (st, decls) decl = do
            (st1, new_decl) <- visitDeclaration (st, decl)
            return (st1, new_decl : decls)
        stmntFun :: (SymbolTable, [Statement]) -> Statement -> Either AnalysisError (SymbolTable, [Statement])
        stmntFun (st, stmnts) stmnt = do
            (st1, new_stmnt) <- visitStatement (st, stmnt)
            return (st1, new_stmnt : stmnts)

visitStatement :: (SymbolTable, Statement) -> Either AnalysisError (SymbolTable, Statement)
visitStatement (st, stmnt@(AssignStatement lhs rhs sourcePos)) = do
    -- visit lhs
    (st1, new_lhs) <- visitExpression (st, lhs)
    -- visit rhs
    (st2, new_rhs) <- visitExpression (st1, rhs)
    -- compare if their types match
    -- for that get their symbols
    lhs_symbol <- maybeToEither (TypeError sourcePos "Symbol for expression not found, how could this happen?!") (symbolForExpression new_lhs st2)
    rhs_symbol <- maybeToEither (TypeError sourcePos "Symbol for expression not found, how could this happen?!") (symbolForExpression new_rhs st2)
    -- get their types
    let lhs_type = symbolType lhs_symbol
    let rhs_type = symbolType rhs_symbol

    -- TODO add casts in some cases
    if lhs_type /= rhs_type 
        then Left $ TypeError sourcePos "Types for assignment don't match!"
        else return (st2, AssignStatement new_lhs new_rhs sourcePos)
visitStatement (st, stmnt@(FunctionCallStatement fun_call sourcePos)) = do
    -- visit funCall
    (st1, new_fun_call) <- visitExpression (st, fun_call)
    return (st1, FunctionCallStatement new_fun_call sourcePos)
visitStatement (st, stmnt@(IfStatement cond then_block m_else_block sourcePos)) = do
    -- visit cond
    (st1, new_cond) <- visitExpression (st, cond)
    -- visit then
    (st2, new_then) <- visitBlock (st1, then_block)
    -- visit else (maybe)
    (st3, new_else) <- case m_else_block of
        Nothing -> return (st2, Nothing)
        Just else_block -> do
                (st, new_block) <- visitBlock (st2, else_block)
                return (st, Just new_block)
    return (st3, IfStatement new_cond new_then new_else sourcePos)
visitStatement (st, stmnt@(WhileStatement cond block sourcePos)) = do
    -- visit cond
    (st1, new_cond) <- visitExpression (st, cond)
    -- visit block
    (st2, new_block) <- visitBlock (st1, block)
    return (st2, WhileStatement new_cond new_block sourcePos)
visitStatement (st, stmnt@(ReturnStatement m_expr sourcePos)) = do
    -- visit expression
    (st1, new_expr) <- case m_expr of
        Nothing -> return (st, Nothing)
        Just expr -> do
            (st, new_expr) <- visitExpression (st, expr)
            return (st, Just new_expr)
    return (st1, ReturnStatement new_expr sourcePos)

visitExpression :: (SymbolTable, Expression) -> Either AnalysisError (SymbolTable, Expression)
visitExpression (st, expr@(ArrayAccess var lengths sourcePos)) = do
    -- visit variable
    (st1, new_var) <- visitExpression (st, var)
    -- get array symbol
    symb@(Symbol symb_ident symb_type symb_decl symb_scope ) <- maybeToEither (TypeError sourcePos "Symbol for expression not found, how could this happen?!") (symbolForExpression new_var st1)
    -- make sure that array var is of variable type and array dimensions fit
    x <- case symb_type of
        ArrayType baseType dimensions | length dimensions == length lengths -> return ()
        ArrayType baseType dimensions -> Left $ TypeError sourcePos ("Indexing a array variable with the wrong dimensions. (Exptected " ++ show (length dimensions) ++ ", Actual " ++ show (length lengths) ++ ")")
        errType -> Left $ TypeError sourcePos $ "Trying to index a non-array variable, symbol is: " ++ show symb
    -- visit length expressions
    (st2, new_lengths) <- foldM lenFun (st1, []) (reverse lengths)
    return (st2, ArrayAccess new_var new_lengths sourcePos)
    where
        lenFun :: (SymbolTable, [Expression]) -> Expression -> Either AnalysisError (SymbolTable, [Expression])
        lenFun (st, exprs) expr = do
            (st1, new_expr) <- visitExpression (st, expr)
            return (st1, new_expr : exprs)
visitExpression (st, expr@(BinaryExpression e1 op e2 sourcePos)) = do
    -- visit left op
    (st1, left) <- visitExpression (st, e1)
    -- visit right op
    (st2, right) <- visitExpression (st1, e2)
    return (st2, BinaryExpression left op right sourcePos)
visitExpression (st, expr@(Constant lit sourcePos)) = return (st, expr)
visitExpression (st, expr@(FunctionCall fun args sourcePos)) = do
    -- visit identifier
    (st1, new_fun) <- visitExpression (st, fun)
    -- visit args
    (st2, new_args) <- foldM argFun (st1, []) (reverse args)
    return (st2, FunctionCall new_fun new_args sourcePos)
    where
        argFun :: (SymbolTable, [Expression]) -> Expression -> Either AnalysisError (SymbolTable, [Expression])
        argFun (st, exprs) expr = do
            (st1, new_expr) <- visitExpression (st, expr)
            return (st1, new_expr : exprs)
visitExpression (st, expr@(Identifier name)) = return (st, expr)
visitExpression (st, expr@(TypeCast e1 tName sourcePos)) = do
    (st1, new_e1) <- visitExpression (st, e1)
    return (st1, TypeCast new_e1 tName sourcePos)
