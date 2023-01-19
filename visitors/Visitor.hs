{-# LANGUAGE MultiParamTypeClasses #-}

-- todo this maybe needs to get removed later...

module Visitor where

import Syntax

class BaseVisitor p r where
  expression :: ASTNode -> p -> r
  declaration :: ASTNode -> p -> r
  binOp :: ASTNode -> p -> r
  literal :: ASTNode -> p -> r
  statement :: ASTNode -> p -> r
  block :: ASTNode -> p -> r
  typeName :: ASTNode -> p -> r
  program :: ASTNode -> p -> r
  prolog :: ASTNode -> p -> r
  epilog :: ASTNode -> p -> r
  beforeChild :: ASTNode -> ASTNode -> p -> r
  afterChild :: ASTNode -> ASTNode -> p -> r

visitChild :: (BaseVisitor p r) => ASTNode -> ASTNode -> p -> r
visitChild parent child param = do
  -- beforeChild
  -- child.accept??
  afterChild parent child param

visitProgram :: (BaseVisitor p r) => ASTNode -> p -> r
visitProgram program param = do
  -- prolog
  -- iterate over declarations and accept them
  epilog program param
