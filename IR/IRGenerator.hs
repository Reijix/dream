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
createVirtualRegister regType = do
    -- get registerNum and currentFun from state
    regNum <- gets nextVirtualRegisterNum
    fun <- gets currentFun
    -- create register
    let register = IRVar ("%" ++ show regNum) False True regType
    -- add register to currentFun
    modify (\state -> state { currentFun = fun { funVirtualRegs = register : funVirtualRegs fun } })
    return register
createLabel :: IRMonad LABEL
createLabel = do
    labelNum <- gets nextLabel
    modify (\state -> state {nextLabel = labelNum + 1})
    return $ LBL labelNum
setLastLabelLeft :: LABEL -> IRMonad ()
setLastLabelLeft label = modify (\state -> state {lastLabelLeft = label})
setLastLabelRight :: LABEL -> IRMonad ()
setLastLabelRight label = modify (\state -> state {lastLabelRight = label})
setIsLValue :: Bool -> IRMonad ()
setIsLValue new = modify (\state -> state {isLValue = new})
createIndexForArrayAccess :: Type -> [IROperand] -> IRMonad IROperand
createIndexForArrayAccess (ArrayType _ dimensions) ops = do
    -- assertion: (length dimensions) == (lengths exprs)
    let zipped = reverse $ zip ops [1..length dimensions]
    let (outerOp, _) = head zipped
    let zipped' = tail zipped
    foldM doAdd outerOp zipped'
    where
        mapFun :: (IROperand, Int) -> IRMonad IROperand
        mapFun (op, idx) = undefined
        doAdd :: IROperand -> (IROperand, Int) -> IRMonad IROperand
        doAdd lastAdd (op, idx) = do
            let dims = Prelude.drop idx dimensions
            multResult <- foldM doMult op dims
            -- create register for result
            register <- createVirtualRegister $ PrimType INT
            appendInstruction . Assignment $ BinaryOperation register multResult lastAdd IRSyntax.ADD 
            return $ IRVariable register
        doMult :: IROperand -> Int -> IRMonad IROperand
        doMult lastMul dimension = do
            -- create register for result
            register <- createVirtualRegister $ PrimType INT
            -- add instruction
            let instruction = Assignment $ BinaryOperation register lastMul (IRConstant $ IRIntConstant dimension) IRSyntax.MUL
            appendInstruction instruction
            return $ IRVariable register

-- helpers for extracting types out of the symbol table
getTypeForDeclaration :: Declaration -> IRMonad Type
getTypeForDeclaration decl = do
    st <- gets symbolTable
    let (Just symbol) = symbolForDeclaration decl st
    return $ symbolType symbol
getTypeForExpression :: Expression -> IRMonad Type
getTypeForExpression expr = do
    st <- gets symbolTable
    let symbol = case symbolForExpression expr st of
            Nothing -> error $ "no symbol for expression: " ++ show expr
            Just s -> s
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
    -- get type
    varType <- getTypeForDeclaration decl
    -- create IRVariable
    createIRVariable name varType

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
visitExpression expr@(ArrayAccess aVar accesses _) = do
    -- save old isLValue and set current one to false
    isLValueOld <- gets isLValue
    setIsLValue False

    -- get array variable
    arrayIROp <- visitExpression aVar

    -- visit accessExpression
    sizeOperands <- mapM visitExpression accesses

    -- restore old isLValue
    setIsLValue isLValueOld

    -- get array type to extract the dimensions
    arrayType <- getTypeForExpression aVar

    -- convert the accesses to a single index in row-major-order
    index <- createIndexForArrayAccess arrayType sizeOperands

    -- if we are an LValue just return the index, otheriwse use a load instruction to fetch the data
    if isLValueOld 
        then return index
        else do
            -- get basetype of arrayaccess
            arrayType <- getTypeForExpression expr
            let baseType = case arrayType of
                    ArrayType baseType _ -> PrimType baseType
            -- create a virtual register to load result into
            register <- createVirtualRegister baseType
            let arrayIRVar = case arrayIROp of IRVariable irVar -> irVar
            -- append LOAD instruction
            appendInstruction $ Assignment $ LOAD register arrayIRVar index
            return $ IRVariable register
visitExpression expr@(FunctionCall (Identifier funName _) args _) = do
    -- visit arguments
    irArguments <- mapM visitExpression args
    
    -- check type of functionCall to see if we return void
    funType <- getTypeForExpression expr
    -- create a return register (if needed) and append the CALL instruction
    case funType of
        VoidType -> do 
            appendInstruction . Assignment $ CALL Nothing funName funType irArguments
            return undefined -- just return some garbage, this never gets used (hopefully hehe)
        _ -> do
            register <- createVirtualRegister funType
            appendInstruction . Assignment $ CALL (Just register) funName funType irArguments
            return $ IRVariable register
visitExpression expr@(Identifier name _) = do
    -- lookup identifier in defined vars and return the associated IRVariable
    definedVars <- gets definedVariables
    symbol <- getSymbolForExpression expr
    let (Just var) = lookup symbol definedVars
    return $ IRVariable var
visitExpression expr@(TypeCast innerExpr (PrimitiveTypeName pType) _) = do
    -- visit innerExpr
    exprOp <- visitExpression innerExpr

    -- create target register
    register <- createVirtualRegister (PrimType pType)

    -- create cast instruction
    let instruction = case pType of
            INT -> Assignment $ CastOperation register exprOp (PrimType REAL) (PrimType INT) R2I
            REAL -> Assignment $ CastOperation register exprOp (PrimType INT) (PrimType REAL) I2R
    appendInstruction instruction
    return $ IRVariable register
visitExpression expr@(Constant (IntLit val) _) = return . IRConstant $ IRIntConstant val
visitExpression expr@(Constant (RealLit val) _) = return . IRConstant $ IRRealConstant val
visitExpression expr@(BinaryExpression leftExpr Syntax.ADD rightExpr _) = visitArithmeticExpression expr
visitExpression expr@(BinaryExpression leftExpr Syntax.SUB rightExpr _) = visitArithmeticExpression expr
visitExpression expr@(BinaryExpression leftExpr Syntax.MUL rightExpr _) = visitArithmeticExpression expr
visitExpression expr@(BinaryExpression leftExpr Syntax.DIV rightExpr _) = visitArithmeticExpression expr
visitExpression expr@(BinaryExpression leftExpr Syntax.AND rightExpr _) = visitLogicalExpression expr
visitExpression expr@(BinaryExpression leftExpr Syntax.OR rightExpr _) = visitLogicalExpression expr
visitExpression expr@(BinaryExpression leftExpr Syntax.EQUALS rightExpr _) = visitCompareExpression expr
visitExpression expr@(BinaryExpression leftExpr Syntax.NOT_EQUALS rightExpr _) = visitCompareExpression expr
visitExpression expr@(BinaryExpression leftExpr Syntax.LESS rightExpr _) = visitCompareExpression expr
visitExpression expr@(BinaryExpression leftExpr Syntax.LESS_EQUALS rightExpr _) = visitCompareExpression expr
visitExpression expr@(BinaryExpression leftExpr Syntax.GREATER rightExpr _) = visitCompareExpression expr
visitExpression expr@(BinaryExpression leftExpr Syntax.GREATER_EQUALS rightExpr _) = visitCompareExpression expr

visitArithmeticExpression :: Expression -> IRMonad IROperand
visitArithmeticExpression expr@(BinaryExpression leftExpr op rightExpr _) = do
    -- visit operands
    leftOp <- visitExpression leftExpr
    rightOp <- visitExpression rightExpr
    -- get type of expression
    exprType <- getTypeForExpression expr
    -- create target register
    register <- createVirtualRegister exprType
    -- create instruction
    let instruction = case op of
            Syntax.ADD -> Assignment $ BinaryOperation register leftOp rightOp IRSyntax.ADD
            Syntax.SUB -> Assignment $ BinaryOperation register leftOp rightOp IRSyntax.SUB
            Syntax.MUL -> Assignment $ BinaryOperation register leftOp rightOp IRSyntax.MUL
            Syntax.DIV -> Assignment $ BinaryOperation register leftOp rightOp IRSyntax.DIV
    appendInstruction instruction
    return $ IRVariable register

visitLogicalExpression :: Expression -> IRMonad IROperand
visitLogicalExpression expr@(BinaryExpression leftExpr Syntax.AND rightExpr _) = do
    -- create new label
    label <- createLabel
    -- save lastlabels to restore later
    lastLabelLeftOld <- gets lastLabelLeft
    lastLabelRightOld <- gets lastLabelRight
    -- code (leftExpr, label, labelFalse)
    setLastLabelLeft label
    visitExpression leftExpr
    -- label
    appendInstruction $ LABEL label
    -- code (rightExpr, labelTrue, labelFalse)
    setLastLabelLeft lastLabelLeftOld
    setLastLabelRight lastLabelRightOld
    visitExpression rightExpr
    -- restore labels
    setLastLabelLeft lastLabelLeftOld
    setLastLabelRight lastLabelRightOld
    return undefined -- result of this shouldn't be used
visitLogicalExpression expr@(BinaryExpression leftExpr Syntax.OR rightExpr _) = do
    -- create new label
    label <- createLabel
    -- save lastlabels to restore later
    lastLabelLeftOld <- gets lastLabelLeft
    lastLabelRightOld <- gets lastLabelRight
    -- code (leftExpr, labelTrue, label)
    setLastLabelRight label
    visitExpression leftExpr
    -- label
    appendInstruction $ LABEL label
    -- code (rightExpr, labelTrue, labelFalse)
    setLastLabelLeft lastLabelLeftOld
    setLastLabelRight lastLabelRightOld
    visitExpression rightExpr
    -- restore labels
    setLastLabelLeft lastLabelLeftOld
    setLastLabelRight lastLabelRightOld
    return undefined -- result of this shouldn't be used

visitCompareExpression :: Expression -> IRMonad IROperand
visitCompareExpression expr@(BinaryExpression leftExpr op rightExpr _) = do
    -- visit operands
    leftOp <- visitExpression leftExpr
    rightOp <- visitExpression rightExpr
    -- get labelTrue and labelFalse
    lastLabelLeft <- gets lastLabelLeft
    lastLabelRight <- gets lastLabelRight
    -- create conditional jump to labelTrue
    let condJump = case op of
            EQUALS -> JEQ
            NOT_EQUALS -> JNE
            LESS -> JLT
            LESS_EQUALS -> JLE
            GREATER -> JGT
            GREATER_EQUALS -> JGE
    let successJump = Jump lastLabelLeft $ ConditionalJump leftOp rightOp condJump
    -- create unconditional jump to labelFalse
    let failureJump = Jump lastLabelRight JMP
    -- append jumps
    appendInstruction successJump
    appendInstruction failureJump
    return undefined -- result shouldn't be used