module ConstantFolding (foldConstants) where

import Syntax

foldConstants :: Program -> Program
foldConstants = visitProgram

visitProgram :: Program -> Program
visitProgram (Program decls) = Program (map visitDeclaration decls)

visitDeclaration :: Declaration -> Declaration
visitDeclaration (FunctionDeclaration expr decls mType block) 
    = FunctionDeclaration (visitExpression expr) (map visitDeclaration decls) mType (visitBlock block)
visitDeclaration (ParameterDeclaration expr tName) = ParameterDeclaration (visitExpression expr) tName
visitDeclaration (VariableDeclaration expr tName) = VariableDeclaration (visitExpression expr) tName

visitBlock :: Block -> Block
visitBlock (Block decls statements) = Block (map visitDeclaration decls) (map visitStatement statements)

visitStatement :: Statement -> Statement
visitStatement (AssignStatement expr1 expr2) = AssignStatement (visitExpression expr1) (visitExpression expr2)
visitStatement (FunctionCallStatement expr) = FunctionCallStatement (visitExpression expr)
visitStatement (IfStatement expr block Nothing) = IfStatement (visitExpression expr) (visitBlock block) Nothing
visitStatement (IfStatement expr block (Just b)) = IfStatement (visitExpression expr) (visitBlock block) (Just (visitBlock b))
visitStatement (WhileStatement expr block) = WhileStatement (visitExpression expr) (visitBlock block)
visitStatement (ReturnStatement Nothing) = ReturnStatement Nothing
visitStatement (ReturnStatement (Just expr)) = ReturnStatement (Just (visitExpression expr))

visitExpression :: Expression -> Expression
visitExpression (ArrayAccess expr exprs) = ArrayAccess (visitExpression expr) (map visitExpression exprs)
visitExpression (BinaryExpression lhs op rhs) = 
    let left = visitExpression lhs
        right = visitExpression rhs
    in
        case (left, right) of 
            (Constant (IntLit l1), Constant (IntLit l2)) -> Constant $ IntLit (applyOp op l1 l2)
            (l, r) -> BinaryExpression l op r
    where
        applyOp ADD = (+)
        applyOp SUB = (-)
        applyOp MUL = (*)
        applyOp DIV = div
        applyOp _ = error "Trying to apply a weird operator to two int constants... this shouldn't happen, your compiler is flawed, idiot!"
visitExpression (FunctionCall expr exprs) = FunctionCall (visitExpression expr) (map visitExpression exprs)
visitExpression (TypeCast expr tName) = TypeCast (visitExpression expr) tName
visitExpression x = x -- Identifier, Constant