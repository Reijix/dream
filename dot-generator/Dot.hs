module Dot where

import Parser
import Syntax
import Control.Monad (foldM)

import System.IO
import GHC.Generics (D)

data DotState = DotState {
    nodeStack :: [Int],
    node_num :: Int
}

createArrow :: Handle -> DotState -> IO DotState
createArrow file (DotState stack currNum) = do
    let (to, newStack) = case stack of 
            [] -> (0,[])
            (x:xs) -> (x,xs)
    let from = case newStack of
            [] -> 0
            (x:xs) -> x
    
    hPutStr file ("  node_" ++ show from ++ " -> node_" ++ show to ++ ";\n")

    return (DotState newStack currNum)

printNode :: Handle -> String -> DotState -> IO DotState
printNode file name (DotState stack currNum) = do
    hPutStr file ("  node_" ++ show currNum ++ " [label=\"" ++ name ++ "\"];\n")
    return (DotState (currNum:stack) (currNum+1))

generateDotFile :: String -> Program -> IO ()
generateDotFile f p = do
    file <- openFile f WriteMode
    hPutStr file "digraph G {\n  graph [ordering=\"out\"];\n"
    newState <- visitProgram (DotState [] 0) file p
    hPutStr file "}"
    hClose file
    

visitProgram :: DotState -> Handle -> Program -> IO DotState
visitProgram state file [] = return state
visitProgram (DotState stack num) file decls = do
    hPutStr file ("  node_" ++ show num ++ " [label=\"Program\"];\n")
    let newState = DotState (num:stack) (num+1)
    foldM (`visitDeclaration` file) newState decls
                

visitDeclaration :: DotState -> Handle -> Declaration -> IO DotState
visitDeclaration state file (GlobalVariableDeclaration varDec) = do
    newState <- printNode file "GlobalVariableDeclaration" state

    retState <- case varDec of
        VariableDeclaration ident tName -> do
            identState <- visitIdentifier newState file ident
            visitTypename identState file tName
    
    createArrow file newState

    return retState
visitDeclaration state file (GlobalFunctionDeclaration funDec) = do
    newState <- printNode file "GlobalFunctionDeclaration" state

    newState <- case funDec of
        FunctionDeclaration ident params retType block -> do
            newState <- visitIdentifier newState file ident
            newState <- foldM (`visitParameterDeclaration` file) newState params
            newState <- case retType of
                Just tName -> visitTypename newState file tName
                Nothing -> return newState
            visitBlock newState file block

    createArrow file newState 

visitParameterDeclaration :: DotState -> Handle -> ParameterDeclaration -> IO DotState
visitParameterDeclaration state file (ParameterDeclaration ident tName) = do
    newState <- printNode file "ParameterDeclaration" state
    newState <- visitIdentifier newState file ident
    newState <- visitTypename newState file tName
    createArrow file newState

visitBlock :: DotState -> Handle -> Block -> IO DotState
visitBlock state file (Block decls statements) = do
    newState <- printNode file "Block" state
    newState <- foldM (`visitVariableDeclaration` file) newState decls
    newState <- foldM (`visitStatement` file) newState statements
    createArrow file newState

visitVariableDeclaration :: DotState -> Handle -> VariableDeclaration -> IO DotState
visitVariableDeclaration state file (VariableDeclaration ident tName) = do
    newState <- printNode file "VariableDeclaration" state
    newState <- visitIdentifier newState file ident
    newState <- visitTypename newState file tName
    createArrow file newState

visitStatement :: DotState -> Handle -> Statement -> IO DotState
visitStatement state file (AssignStatement lvalue aexpr) = do
    newState <- printNode file "AssignStatement" state
    newState <- visitLValue newState file lvalue
    newState <- visitAdditiveExpr newState file aexpr
    createArrow file newState
-- TODO
visitStatement state file (IfStatement cond thenBlock elseBlock) = return state
visitStatement state file (WhileStatement cond thenBlock) = return state
visitStatement state file (ReturnStatement aexpr) = return state
visitStatement state file (FunctionCallStatement funCall) = return state

visitLValue :: DotState -> Handle -> LValue -> IO DotState
visitLValue state file (LValueIdentifier ident) = visitIdentifier state file ident
-- TODO
visitLValue state file (LValueArray aAccess) = return state

visitIdentifier :: DotState -> Handle -> Identifier -> IO DotState
visitIdentifier state file ident = do
    newState <- printNode file "Identifier" state
    createArrow file newState

visitTypename :: DotState -> Handle -> TypeName -> IO DotState
visitTypename state file (TypeName pType accesses) = do
    newState <- printNode file "TypeName" state
    newState <- visitPrimitiveTypename newState file pType
    foldM (`visitAdditiveExpr` file) newState accesses
    createArrow file newState

visitPrimitiveTypename :: DotState -> Handle -> PrimitiveTypeName -> IO DotState
visitPrimitiveTypename state file pType = do
    newState <- case pType of
        INT -> printNode file "INT" state
        REAL -> printNode file "REAL" state
    createArrow file newState
    
visitAdditiveExpr :: DotState -> Handle -> AdditiveExpr -> IO DotState
visitAdditiveExpr state file (AdditiveSimple mul) = do
    newState <- printNode file "AdditiveExpression" state
    newState <- visitMultiplicativeExpr newState file mul
    createArrow file newState
visitAdditiveExpr state file (AdditiveComplexExpr lhs op rhs) = do
    newState <- printNode file "AdditiveExpression" state
    newState <- visitAdditiveExpr newState file lhs
    newState <- visitAdditiveOp newState file op
    newState <- visitAdditiveExpr newState file rhs
    createArrow file newState

visitAdditiveOp :: DotState -> Handle -> AdditiveOp -> IO DotState
visitAdditiveOp state file op = do
    newState <- case op of
        ADD -> printNode file "+" state
        SUB -> printNode file "-" state
    createArrow file newState

visitMultiplicativeExpr :: DotState -> Handle -> MultiplicativeExpr -> IO DotState
visitMultiplicativeExpr state file (MultiplicativeFactor factor) = do
    newState <- printNode file "MultiplicativeExpression" state
    newState <- visitFactor newState file factor
    createArrow file newState
visitMultiplicativeExpr state file (MultiplicativeComplexExpr lhs op rhs) = do
    newState <- printNode file "MultiplicativeExpression" state
    newState <- visitMultiplicativeExpr newState file lhs
    newState <- visitMultiplicativeOp newState file op
    newState <- visitMultiplicativeExpr newState file rhs
    createArrow file newState

visitMultiplicativeOp :: DotState -> Handle -> MultiplicativeOp -> IO DotState
visitMultiplicativeOp state file op = do
    newState <- case op of
        MUL -> printNode file "*" state
        DIV -> printNode file "/" state
    createArrow file newState

visitFactor :: DotState -> Handle -> Factor -> IO DotState
visitFactor state file (FactorVariableAccess vAccess) = return state
visitFactor state file (FactorNumberLiteral numLit) = do
    newState <- case numLit of
        IntLit n -> printNode file (show n) state
        RealLit n -> printNode file (show n) state
        CharLit n -> printNode file (show n) state
    createArrow file newState
-- TODO
visitFactor state file (FactorFunctionCall funcCall) = return state
visitFactor state file (FactorArrayAccess aAccess) = return state
visitFactor state file (FactorArithmeticExpr aExpr) = return state
visitFactor state file (FactorCastExpr cast) = return state

