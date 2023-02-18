module TypeAnalysis where

import SymbolTable
import AnalysisError
import Syntax
import Symbol
import Data.Foldable (Foldable(foldr'))
import Data.Maybe (fromMaybe)
import Control.Monad (foldM, when)
import Data.Either.Extra

-- TODO maybe symboltable can be refactored, the indirection seems useless...

-- helpers for getting the type of a expression
getTypeForLhs :: SymbolTable -> Expression -> Either AnalysisError Type
getTypeForLhs st expr@(Identifier name sourcePos) = do
    (Symbol _ sType _ _) <- maybeToEither (TypeError sourcePos "No symbol for identifier found.") (symbolForExpression expr st)
    return sType
getTypeForLhs st expr@(ArrayAccess varName exprs sourcePos) = do
    aType <- getTypeForLhs st varName
    case aType of
        ArrayType baseType lengths -> return $ PrimType baseType
        _ -> Left $ TypeError sourcePos "Array variable doesn't have array type!"
getTypeForLhs st (BinaryExpression _ _ _ sourcePos) = Left $ TypeError sourcePos "lhs can't be a binaryexpression!" 
getTypeForLhs st (Constant _ sourcePos) = Left $ TypeError sourcePos "lhs can't be a constant!" 
getTypeForLhs st (FunctionCall _ _ sourcePos) = Left $ TypeError sourcePos "lhs can't be a functioncall!" 
getTypeForLhs st (TypeCast _ _ sourcePos) = Left $ TypeError sourcePos "lhs can't be a typecast!" 

getTypeForRhs :: SymbolTable -> Expression -> Either AnalysisError Type
getTypeForRhs st expr@(ArrayAccess var exprs sourcePos) = do
    aType <- getTypeForRhs st var
    case aType of
        ArrayType baseType lengths -> return $ PrimType baseType
        _ -> Left $ TypeError sourcePos "Array variable doesn't have array type!"
getTypeForRhs st expr@(BinaryExpression e1 _ _ sourcePos) = getTypeForRhs st e1
getTypeForRhs st expr@(Constant (IntLit _) sourcePos) = return $ PrimType INT
getTypeForRhs st expr@(Constant (RealLit _) sourcePos) = return $ PrimType REAL
getTypeForRhs st expr@(FunctionCall funName _ sourcePos) = do
    funType <- getTypeForRhs st funName
    case funType of
        FunctionType retType argTypes -> return retType
        _ -> Left $ TypeError sourcePos "FunctionCall on non-function variable!"
getTypeForRhs st expr@(Identifier name sourcePos) = do
    (Symbol _ sType _ _) <- maybeToEither (TypeError sourcePos "No symbol for identifier found") (symbolForExpression expr st)
    return sType
getTypeForRhs st expr@(TypeCast e1 (PrimitiveTypeName INT) sourcePos) = return $ PrimType INT
getTypeForRhs st expr@(TypeCast e1 (PrimitiveTypeName REAL) sourcePos) = return $ PrimType REAL
getTypeForRhs st expr@(TypeCast e1 castType sourcePos) = Left $ TypeError sourcePos "Found cast without primitive type, this shouldn't be possible!"

-- Does type analysis on an AST using the SymbolTable that was constructed during NameAnalysis
-- The type analysis returns a new SymbolTable containing Type information and a new AST containing casts where necessary
doTypeAnalysis :: SymbolTable -> Program -> Either AnalysisError (SymbolTable, Program)
doTypeAnalysis st prog = do
    (st, new_prog) <- visitProgram (st, prog)
    return (st, new_prog)


visitProgram :: (SymbolTable, Program) -> Either AnalysisError (SymbolTable, Program)
visitProgram (st, Program decls) = do
    -- visit declarations first
    (st1, prog1) <- foldM f (st, Program []) (reverse globalVariables)
    -- TODO visit functions shallow before visiting 'for real'
    st2 <- foldM shallowVisit st1 functionDeclarations
    -- then visit functions
    (st2, prog2) <- foldM f (st1, prog1) (reverse functionDeclarations)
    return (st2, prog2)
    where
        globalVariables = [var | var@(VariableDeclaration {}) <- decls]
        functionDeclarations = [fun | fun@(FunctionDeclaration {}) <- decls]
        f :: (SymbolTable, Program) -> Declaration -> Either AnalysisError (SymbolTable, Program)
        f (st, Program decls) decl = do
            (st1, new_decl) <- visitDeclaration (st, decl)
            return (st1, Program (new_decl : decls))
        shallowVisit :: SymbolTable -> Declaration -> Either AnalysisError SymbolTable
        shallowVisit st decl@(FunctionDeclaration funName params mRetType block sourcePos) = do
            -- check return type
            return_type <- get_return_type
            -- visit parameters
            (st1, new_params) <- foldM paramFoldFun (st, []) (reverse params)
            -- get parameterTypes
            let param_types = map extract_type params
            -- construct functionType
            let funtype = FunctionType return_type param_types
            -- get old symbol
            (Symbol old_ident old_type old_decl old_scope ) <- maybeToEither (TypeError sourcePos "Symbol for function not found, how could this happen?!") (symbolForDeclaration decl st)
            -- construct new symbol
            let new_symbol = Symbol old_ident funtype decl old_scope
            -- update symboltable symboltable
            return $ updateDeclarationSymbol decl new_symbol st1
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
                get_return_type = case mRetType of
                    Nothing -> Right VoidType
                    Just (PrimitiveTypeName pType) -> Right $ PrimType pType
                    Just (ArrayTypeName {}) -> Left $ TypeError sourcePos "Function return type must be primitive!"

visitDeclaration :: (SymbolTable, Declaration) -> Either AnalysisError (SymbolTable, Declaration)
visitDeclaration (st, decl@(FunctionDeclaration ident@(Identifier name iSourcePos) params retType block sourcePos)) = do
    -- visit block
    (st1, new_block) <- visitBlock (st, block)
    -- construct new declaration node
    let new_decl = FunctionDeclaration ident params retType new_block sourcePos
    -- get old symbol
    (Symbol old_ident old_type old_decl old_scope ) <- maybeToEither (TypeError sourcePos "Symbol for function not found, how could this happen?!") (symbolForDeclaration decl st)
    -- construct new symbol
    let new_symbol = Symbol old_ident old_type new_decl old_scope
    -- add new symbol to symboltable
    let st2 = insertDeclarationSymbol new_decl new_symbol st1
    -- TODO remove old symbol...
    return (st2, new_decl)

visitDeclaration (st, decl@(ParameterDeclaration ident@(Identifier name iSourcePos) paramType sourcePos)) = do
    -- get type
    param_type <- get_param_type paramType
    -- get old symbol
    (Symbol old_ident old_type old_decl old_scope ) <- maybeToEither (TypeError sourcePos "Symbol for function not found, how could this happen?!") (symbolForDeclaration decl st)
    -- construct new symbol
    let new_symbol = Symbol old_ident param_type decl old_scope
    -- update symboltable
    let st1 = updateDeclarationSymbol decl new_symbol st
    return (st1, decl)
    where
        get_param_type :: TypeName -> Either AnalysisError Type
        get_param_type paramType = do 
            case paramType of
                PrimitiveTypeName primType -> Right $ PrimType primType
                _ -> Left $ TypeError sourcePos "Function parameters must have primitive type!"
visitDeclaration (st, decl@(VariableDeclaration ident@(Identifier name iSourcePos) varType sourcePos)) = do
    -- get type
    (st1, var_type) <- get_var_type st varType
    -- get old symbol
    (Symbol old_ident old_type old_decl old_scope ) <- maybeToEither (TypeError sourcePos "Symbol for variable not found, how could this happen?!") (symbolForDeclaration decl st1)
    -- construct new symbol
    let new_symbol = Symbol old_ident var_type decl old_scope
    -- update symboltable
    let st2 = updateDeclarationSymbol decl new_symbol st1
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
    lhs_type <- getTypeForLhs st2 new_lhs
    rhs_type <- getTypeForRhs st2 new_rhs

    -- TODO add casts in some cases
    if lhs_type /= rhs_type 
        then Left $ TypeError sourcePos $ "Types for assignment don't match!\nLeft: " ++ show lhs_type ++ "\nRight: " ++ show rhs_type 
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
    -- TODO check that returnType matches, needs some state to be passed in the visitor
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
    -- TODO check that types match
    -- visit identifier
    (st1, new_fun) <- visitExpression (st, fun)
    -- visit args
    (st2, new_args) <- foldM argFun (st1, []) (reverse args)
    -- make sure that types match
    funType <- getTypeForRhs st2 new_fun
    argTypes <- mapM (getTypeForRhs st2) new_args
    case funType of
        FunctionType retType expectedArgTypes -> checkArgTypes expectedArgTypes argTypes
        _ -> Left $ TypeError sourcePos $ "functioncall on non-function variable, type is: " ++ show funType ++ "\nCurrent symbolTable is:\n" ++ showSymbolTable st2
    return (st2, FunctionCall new_fun new_args sourcePos)
    where
        checkArgTypes :: [Type] -> [Type] -> Either AnalysisError ()
        checkArgTypes expected actual = do 
            when (length expected /= length actual) $ Left $ TypeError sourcePos "Number of arguments for functionCall doesn't match!"
            let zipped = zip expected actual
            foldM (\x (a,b) -> (if a == b then return x else Left $ TypeError sourcePos "Argument types do not match!")) () zipped
        argFun :: (SymbolTable, [Expression]) -> Expression -> Either AnalysisError (SymbolTable, [Expression])
        argFun (st, exprs) expr = do
            (st1, new_expr) <- visitExpression (st, expr)
            return (st1, new_expr : exprs)
visitExpression (st, expr@(Identifier name iSourcePos)) = return (st, expr)
visitExpression (st, expr@(TypeCast e1 tName sourcePos)) = do
    (st1, new_e1) <- visitExpression (st, e1)
    return (st1, TypeCast new_e1 tName sourcePos)
