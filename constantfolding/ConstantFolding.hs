module ConstantFolding (foldConstants) where

import Syntax
import Data.Char (digitToInt)

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
visitExpression expr@(BinaryExpression lhs op rhs sourcePos) | isArithOp op =
  let left = visitExpression lhs
      right = visitExpression rhs
   in case (left, right) of
        (Constant (IntLit l1) sourcePos1, Constant (IntLit l2) sourcePos2) -> Constant (IntLit (applyOp op l1 l2)) sourcePos -- fold
        (Constant (CharLit l1) sourcePos1, Constant (IntLit l2) sourcePos2) -> Constant (IntLit (applyOp op (digitToInt l1) l2)) sourcePos -- fold
        (Constant (IntLit l1) sourcePos1, Constant (CharLit l2) sourcePos2) -> Constant (IntLit (applyOp op l1 (digitToInt l2))) sourcePos -- fold
        (Constant (CharLit l1) sourcePos1, Constant (CharLit l2) sourcePos2) -> Constant (IntLit (applyOp op (digitToInt l1) (digitToInt l2))) sourcePos -- fold
        _ -> expr -- no folding
  where
    applyOp ADD = (+)
    applyOp SUB = (-)
    applyOp MUL = (*)
    applyOp DIV = div
    applyOp _ = error "No binary expression found while constantfolding..."
visitExpression expr@(BinaryExpression lhs op rhs sourcePos) = BinaryExpression (visitExpression lhs) op (visitExpression rhs) sourcePos
visitExpression (FunctionCall expr exprs sourcePos) = FunctionCall (visitExpression expr) (map visitExpression exprs) sourcePos
visitExpression (TypeCast expr tName sourcePos) = TypeCast (visitExpression expr) tName sourcePos
visitExpression x = x -- Identifier, Constant

isArithOp :: BinOp -> Bool
isArithOp ADD = True
isArithOp SUB = True
isArithOp MUL = True
isArithOp DIV = True
isArithOp _ = False
