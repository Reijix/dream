module ConstantFolding (foldConstants) where

import Syntax

foldConstants :: Program -> Program
foldConstants = visitProgram

visitProgram :: Program -> Program
visitProgram (Program decls) = Program (map visitDeclaration decls)

visitDeclaration :: Declaration -> Declaration
visitDeclaration (FunctionDeclaration expr decls mType block sourcePos) =
  FunctionDeclaration (visitExpression expr) (map visitDeclaration decls) mType (visitBlock block) sourcePos
visitDeclaration (ParameterDeclaration expr tName sourcePos) = ParameterDeclaration (visitExpression expr) tName sourcePos
visitDeclaration (VariableDeclaration expr tName sourcePos) = VariableDeclaration (visitExpression expr) tName sourcePos

visitBlock :: Block -> Block
visitBlock (Block decls statements) = Block (map visitDeclaration decls) (map visitStatement statements)

visitStatement :: Statement -> Statement
visitStatement (AssignStatement expr1 expr2 sourcePos) = AssignStatement (visitExpression expr1) (visitExpression expr2) sourcePos
visitStatement (FunctionCallStatement expr sourcePos) = FunctionCallStatement (visitExpression expr) sourcePos
visitStatement (IfStatement expr block Nothing sourcePos) = IfStatement (visitExpression expr) (visitBlock block) Nothing sourcePos
visitStatement (IfStatement expr block (Just b) sourcePos) = IfStatement (visitExpression expr) (visitBlock block) (Just (visitBlock b)) sourcePos
visitStatement (WhileStatement expr block sourcePos) = WhileStatement (visitExpression expr) (visitBlock block) sourcePos
visitStatement (ReturnStatement Nothing sourcePos) = ReturnStatement Nothing sourcePos
visitStatement (ReturnStatement (Just expr) sourcePos) = ReturnStatement (Just (visitExpression expr)) sourcePos

visitExpression :: Expression -> Expression
visitExpression (ArrayAccess expr exprs sourcePos) = ArrayAccess (visitExpression expr) (map visitExpression exprs) sourcePos
visitExpression expr@(BinaryExpression lhs op rhs sourcePos) =
  let left = visitExpression lhs
      right = visitExpression rhs
   in case (left, right) of
        (Constant (IntLit l1) sourcePos1, Constant (IntLit l2) sourcePos2) -> Constant (IntLit (applyOp op l1 l2)) sourcePos -- fold
        _ -> expr -- no folding
  where
    applyOp ADD = (+)
    applyOp SUB = (-)
    applyOp MUL = (*)
    applyOp DIV = div
    applyOp _ = error "Trying to apply a weird operator to two int constants... this shouldn't happen, your compiler is flawed, idiot!"
visitExpression (FunctionCall expr exprs sourcePos) = FunctionCall (visitExpression expr) (map visitExpression exprs) sourcePos
visitExpression (TypeCast expr tName sourcePos) = TypeCast (visitExpression expr) tName sourcePos
visitExpression x = x -- Identifier, Constant
