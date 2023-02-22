module IRGenerator where

import Control.Monad.State
import Syntax
import IRSyntax
import SymbolTable
import Symbol
import Data.Foldable ( foldl' )
import Data.Map
import Prelude hiding ( lookup )
import Data.Maybe (fromJust)


{-
NOTE: 
in here we can and do make many assumptions (e.g. each declaration has a symbol, types are fitting etc.)
When the compiler fails because of non-exhaustive patterns,
it means that a earlier step (name analysis or type analysis) is broken, not this one!
-}

data IRState = IRState {
    variableNumbering :: Map String Int,
    nextVirtualRegisterNum :: Int,
    definedVariables :: Map Symbol IRVariable,
    symbolTable :: SymbolTable,
    lastLabelLeft :: LABEL,
    lastLabelRight :: LABEL,
    isLValue :: Bool,
    currentFun :: IRFunction
}

type IRMonad = State IRState

createIRVariable :: String -> Type -> IRMonad IRVariable
createIRVariable name varType = do
    numbering <- gets variableNumbering
    idx <- case lookup name numbering of
        Nothing -> do modify (\state -> state {variableNumbering = insert name 1 numbering}); return 0
        Just idx -> do modify (\state -> state {variableNumbering = insert name (idx + 1) numbering}); return idx
    return $ IRVar (name ++ "$" ++ show idx) False False varType

createVirtualRegister :: Type -> IRMonad IRVariable
createVirtualRegister regType = undefined

-- helpers for extracting types out of the symbol table
getTypeForDeclaration :: Declaration -> IRMonad Type
getTypeForDeclaration decl = do
    st <- gets symbolTable
    let (Just symbol) = symbolForDeclaration decl st
    return $ symbolType symbol
getSymbolForDeclaration :: Declaration -> IRMonad Symbol
getSymbolForDeclaration decl = do
    st <- gets symbolTable
    return . fromJust $ symbolForDeclaration decl st


-- runner function that generates the IR AST for a given dream AST
generateIR :: SymbolTable -> Program -> IRProgram
generateIR st prog = evalState (visitProgram prog) initialState
    where
        initialState = IRState empty 0 empty st dummyLabel dummyLabel False dummyFunction
        dummyFunction = IRFunction "" VoidType [] [] [] []
        dummyLabel = LBL ""

visitProgram :: Program -> IRMonad IRProgram
visitProgram (Program decls) = do
    irVars <- mapM visitGlobalVariable globalVariables
    irFunctions <- mapM visitFunctionDeclaration functionDeclarations
    return $ IRProgram irVars irFunctions
    where
        globalVariables = [var | var@(VariableDeclaration {}) <- decls]
        functionDeclarations = [fun | fun@(FunctionDeclaration {}) <- decls]

visitGlobalVariable :: Declaration -> IRMonad IRVariable
visitGlobalVariable decl@(VariableDeclaration (Identifier name _) _ _) = do
    -- create global IRVar
    declType <- getTypeForDeclaration decl
    let irVar = IRVar name True False declType
    -- insert symbol into definedVariables
    symbol <- getSymbolForDeclaration decl
    modify (\state -> state {definedVariables = insert symbol irVar (definedVariables state)})
    return irVar

visitLocalVariable :: Declaration -> IRMonad IRVariable
visitLocalVariable decl@(VariableDeclaration (Identifier name _) _ _) = do

    return undefined

visitParameterDeclaration :: Declaration -> IRMonad IRVariable
visitParameterDeclaration decl@(ParameterDeclaration (Identifier name _) _ _) = do
    -- create local IRVar
    declType <- getTypeForDeclaration decl
    irVar <- createIRVariable name declType
    -- insert symbol into definedVariables
    symbol <- getSymbolForDeclaration decl
    modify (\state -> state {definedVariables = insert symbol irVar (definedVariables state)})
    return irVar

visitFunctionDeclaration :: Declaration -> IRMonad IRFunction
visitFunctionDeclaration decl@(FunctionDeclaration (Identifier name _) params _ block _) = do
    -- get returnType of function
    retM <- getTypeForDeclaration decl
    retType <- case retM of
        FunctionType ret _ -> return ret
        _ -> error "Function has non function type, this is an error of the dream compiler!!!"

    -- visit parameters
    paramVars <- mapM visitParameterDeclaration params

    -- insert current function into state
    modify (\state -> state {currentFun = IRFunction name retType [] paramVars [] []})

    -- visit block
    instructions <- visitBlock block

    fun <- gets currentFun 
    return fun {funInstructions = instructions}

visitBlock :: Block -> IRMonad [IRInstruction]
visitBlock (Block decls stmnts) = do
    -- visit local variables and insert into state
    localVars <- mapM visitLocalVariable decls
    modify (\state -> state {currentFun = (currentFun state) {funLocalVars = localVars} })

    -- visit statements and return their instructions
    foldM foldStatements [] stmnts
    where
        foldStatements :: [IRInstruction] -> Statement -> IRMonad [IRInstruction]
        foldStatements instructions stmnt = do
            stmntInstructions <- visitStatement stmnt
            return $ instructions ++ stmntInstructions

visitStatement :: Statement -> IRMonad [IRInstruction]
visitStatement = undefined