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
    newState <- hPutStr file ("  node_" ++ show currNum ++ " [label=\"" ++ name ++ "\"];\n")
    return (DotState (currNum:stack) (currNum+1))

generateDotFile :: String -> Program -> IO ()
generateDotFile f p = do
    file <- openFile f WriteMode
    hPutStr file "digraph G {\n  graph [ordering=\"out\"];\n"
    newState <- visitProgram (DotState [] 0) file p
    hPutStr file "}"
    hClose file
    

visitProgram :: DotState -> Handle -> Program -> IO DotState
visitProgram state file (Program decls) = do
    newState <- printNode file "Program" state
    foldM (`visitDeclaration` file) newState decls

visitDeclaration :: DotState -> Handle -> Declaration -> IO DotState
visitDeclaration state file (VariableDeclaration ident typeName) = do
    newState <- printNode file "VariableDeclaration" state
    newState <- visitExpression newState file ident
    newState <- visitTypeName newState file typeName
    createArrow file newState
visitDeclaration state file (FunctionDeclaration ident params retType block) = do
    newState <- printNode file "FunctionDeclaration" state
    newState <- visitExpression newState file ident
    newState <- foldM (`visitDeclaration` file) newState params
    newState <- case retType of
        Nothing -> return newState
        Just rType -> visitTypeName newState file rType
    newState <- visitBlock newState file block
    createArrow file newState
visitDeclaration state file (ParameterDeclaration ident typeName) = do
    newState <- printNode file "ParameterDeclaration" state
    newState <- visitExpression newState file ident
    newState <- visitTypename newState file typeName
    createArrow file newState

visitBlock :: DotState -> Handle -> Block -> IO DotState
visitBlock state file (Block decls statements) = do
    newState <- printNode file "Block" state
    newState <- foldM (`visitDeclaration` file) newState decls
    newState <- foldM (`visitStatement` file) newState statements
    createArrow file newState

visitStatement :: DotState -> Handle -> Statement -> IO DotState
visitStatement state file (AssignStatement lhs rhs) = do
    newState <- printNode file "AssignStatement" state
    newState <- visitExpression newState file lhs
    newState <- visitExpression newState file rhs
    createArrow file newState
visitStatement state file (FunctionCallStatement funCall) = do
    newState <- printNode file "FunctionCallStatement" state
    newState <- visitExpression newState file funCall
    createArrow file newState
visitStatement state file (IfStatement cond thenBlock elseBlock) = do
    newState <- printNode file "IfStatement" state
    newState <- visitExpression newState file cond
    newState <- visitBlock newState file thenBlock
    newState <- case elseBlock of
        Nothing -> return newState
        Just tBlock -> visitBlock newState file tBlock
    createArrow file newState
visitStatement state file (WhileStatement cond thenBlock) = do
    newState <- printNode file "WhileStatement" state
    newState <- visitExpression newState file cond
    newState <- visitBlock newState file thenBlock
    createArrow file newState
visitStatement state file (ReturnStatement aexpr) = do
    newState <- printNode file "ReturnStatement" state
    newState <- case aexpr of
        Nothing -> return newState
        Just expr -> visitExpression newState file expr
    createArrow file newState

visitTypeName :: DotState -> Handle -> TypeName -> IO DotState
visitTypeName state file (PrimitiveTypeName pType) = do
    newState <- case pType of
        INT -> printNode file "<int>" state
        REAL -> printNode file "<real>" state
    createArrow file newState
visitTypename state file (ArrayTypeName pType accesses) = do
    newState <- visitTypeName state file pType
    newState <- foldM (`visitExpression` file) newState accesses
    createArrow file newState

visitExpression :: DotState -> Handle -> Expression -> IO DotState
visitExpression state file (ArrayAccess ident accesses) = do
    newState <- printNode file "ArrayAccess" state
    newState <- visitExpression newState file ident
    newState <- foldM (`visitExpression` file) newState accesses
    createArrow file newState
visitExpression state file (BinaryExpression lop op rop) = do
    newState <- printNode file "BinaryExpression" state
    newState <- visitExpression newState file lop
    newState <- visitBinOp newState file op
    newState <- visitExpression newState file rop
    createArrow file newState
visitExpression state file (Constant lit) = do
    newState <- case lit of
        IntLit n -> printNode file (show n) state
        RealLit n -> printNode file (show n) state
        CharLit n -> printNode file (show n) state
    createArrow file newState
visitExpression state file (FunctionCall ident args) = do
    newState <- printNode file "FunctionCall" state
    newState <- visitExpression newState file ident
    newState <- foldM (`visitExpression` file) newState args
    createArrow file newState
visitExpression state file (Identifier name) = do
    newState <- printNode file name state
    createArrow file newState
visitExpression state file (TypeCast expression newType) = do
    newState <- printNode file "CastExpression" state
    newState <- visitExpression newState file expression
    newState <- visitTypeName newState file newType
    createArrow file newState

visitBinOp :: DotState -> Handle -> BinOp -> IO DotState
visitBinOp state file op = do
    newState <- case op of
        EQUALS -> printNode file "==" state
        NOT_EQUALS -> printNode file "!=" state
        LESS -> printNode file "<" state
        LESS_EQUALS -> printNode file "<=" state
        GREATER -> printNode file ">" state
        GREATER_EQUALS -> printNode file ">=" state
        ADD -> printNode file "+" state
        SUB -> printNode file "-" state
        MUL -> printNode file "*" state
        DIV -> printNode file "/" state
        AND -> printNode file "and" state
        OR -> printNode file "or" state
    createArrow file newState
