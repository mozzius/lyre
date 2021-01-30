module Exprs where

type Stmts = [Stmt]

data Stmt
  = Let String Expr
  | Return Expr
  | FuncDef String [String] Block
  deriving (Show)

data BinOp
  = Or
  | And
  | Plus
  | Minus
  | Div
  | Times
  deriving (Show)

data UnaOp
  = Inv
  deriving (Show)

data Expr
  = BinOp BinOp Expr Expr
  | UnaOp UnaOp Expr
  | Int Int
  | Var String
  | Brack Expr
  | FuncCall String [Expr]
  deriving (Show)

data Block
  = Curly Stmts
  | Expr Expr
  deriving (Show)
