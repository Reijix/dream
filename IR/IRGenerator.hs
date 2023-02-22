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
    nextLabel :: Int,
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

-- helpers for modifying state
appendInstruction :: IRInstruction -> IRMonad ()
appendInstruction instruction = do
    fun <- gets currentFun
    modify (\state -> state {currentFun = fun {funInstructions = funInstructions fun ++ [instruction]} })
createIRVariable :: String -> Type -> IRMonad IRVariable
createIRVariable name varType = do
    numbering <- gets variableNumbering
    idx <- case lookup name numbering of
        Nothing -> do modify (\state -> state {variableNumbering = insert name 1 numbering}); return 0
        Just idx -> do modify (\state -> state {variableNumbering = insert name (idx + 1) numbering}); return idx
    return $ IRVar (name ++ "$" ++ show idx) False False varType
createVirtualRegister :: Type -> IRMonad IRVariable
createVirtualRegister regType = undefined
createLabel :: IRMonad LABEL
createLabel = do
    labelNum <- gets nextLabel
    modify (\state -> state {nextLabel = labelNum + 1})
    return $ LBL labelNum
setLastLabelLeft :: LABEL -> IRMonad ()
setLastLabelLeft label = modify (\state -> state {lastLabelLeft = label})
setLastLabelRight :: LABEL -> IRMonad ()
setLastLabelRight label = modify (\state -> state {lastLabelRight = label})

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
getSymbolForExpression :: Expression -> IRMonad Symbol
getSymbolForExpression expr = do
    st <- gets symbolTable
    return . fromJust $ symbolForExpression expr st

-- runner function that generates the IR AST for a given dream AST
generateIR :: SymbolTable -> Program -> IRProgram
generateIR st prog = evalState (visitProgram prog) initialState
    where
        initialState = IRState 0 empty 0 empty st dummyLabel dummyLabel False dummyFunction
        dummyFunction = IRFunction "" VoidType [] [] [] []
        dummyLabel = LBL (-1)

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

    -- return currentFun
    gets currentFun 

visitBlock :: Block -> IRMonad ()
visitBlock (Block decls stmnts) = do
    -- visit local variables and insert into state
    localVars <- mapM visitLocalVariable decls
    modify (\state -> state {currentFun = (currentFun state) {funLocalVars = localVars} })

    -- visit statements
    foldM_ (\b a -> visitStatement a) () stmnts

visitStatement :: Statement -> IRMonad ()
visitStatement (AssignStatement lhs rhs _) = do
    -- visit rhs first (see spec)
    modify (\state -> state {isLValue = False})
    rhsOperand <- visitExpression rhs

    -- visit lhs (and prepare state before doing so)
    modify (\state -> state {isLValue = True})
    lhsOperand <- visitExpression lhs

    -- build instruction for assignment, either MOV or STORE
    instruction <- case lhs of
            expr@(ArrayAccess aVar aLens _) -> do
                arraySymbol <- getSymbolForExpression expr
                vars <- gets definedVariables
                let arrayVariable = fromJust $ lookup arraySymbol vars
                return $ STORE arrayVariable lhsOperand rhsOperand
            _ -> case lhsOperand of
                    IRVariable irVar -> return . Assignment $ MOV irVar rhsOperand
                    _ -> error "Error in compiler, lhs in assignmentStatement is not IRVariable!"

    -- add instruction to list
    appendInstruction instruction
visitStatement (FunctionCallStatement funCall _) = do 
    visitExpression funCall
    return ()
visitStatement (IfStatement cond thenBlock Nothing _) = do
    -- create labels
    labelTrue <- createLabel
    labelFalse <- createLabel
    -- code (cond, labelTrue, labelFalse)
    setLastLabelLeft labelTrue
    setLastLabelRight labelFalse
    visitExpression cond
    -- labelTrue
    appendInstruction $ LABEL labelTrue
    -- code (thenBlock)
    visitBlock thenBlock
    -- labelFalse
    appendInstruction $ LABEL labelFalse
visitStatement (IfStatement cond thenBlock (Just elseBlock) _) = do
    -- create labels
    labelTrue <- createLabel
    labelFalse <- createLabel
    labelEnd <- createLabel
    -- code (cond, labelTrue, labelFalse)
    setLastLabelLeft labelTrue
    setLastLabelRight labelFalse
    visitExpression cond
    -- labelTrue
    appendInstruction $ LABEL labelTrue
    -- code (thenBlock)
    visitBlock thenBlock
    -- jmp labelEnd
    appendInstruction $ Jump labelEnd JMP
    -- labelFalse
    appendInstruction $ LABEL labelFalse
    -- code(elseBlock)
    visitBlock elseBlock
    -- labelEnd
    appendInstruction $ LABEL labelEnd
visitStatement (WhileStatement cond block _) = do
    -- create labels
    labelTrue <- createLabel
    labelFalse <- createLabel
    labelCond <- createLabel
    -- jmp labelCond
    appendInstruction $ Jump labelCond JMP
    -- labelTrue
    appendInstruction $ LABEL labelTrue
    -- code(block)
    visitBlock block
    -- labelCond
    appendInstruction $ LABEL labelCond
    -- code(cond, labelTrue, labelFalse)
    setLastLabelLeft labelTrue
    setLastLabelRight labelFalse
    visitExpression cond
    -- labelFalse
    appendInstruction $ LABEL labelFalse
visitStatement (ReturnStatement Nothing _) = appendInstruction $ RET Nothing
visitStatement (ReturnStatement (Just value) _) = do
    expr <- visitExpression value
    appendInstruction $ RET (Just expr)

visitExpression :: Expression -> IRMonad IROperand
visitExpression = undefined