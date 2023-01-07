module Syntax where

type Name = String

data Expr
    = Float Double
    | BinOp Op Expr Expr
    | Var String
    deriving (Eq, Ord, Show)

data Op
    = Plus
    | Minus
    deriving (Eq, Ord, Show)