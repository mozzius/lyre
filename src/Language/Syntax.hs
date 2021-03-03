module Language.Syntax where

type Stmts = [Stmt]

data Stmt
  = Let String Type Expr
  | Return Expr
  | FuncDef String [Argument] Type Block
  | If Expr Block
  | IfElse Expr Block Block
  | IfElseIf Expr Block Stmt
  deriving (Show)

data Argument
  = Arg String Type
  deriving (Show)

data BinOp
  = Or
  | And
  | Plus
  | Minus
  | Div
  | Times
  | Equals
  | GreaterThan
  | LessThan
  | GreaterEq
  | LessEq
  | NotEquals
  deriving (Show)

data UnaOp
  = Inv
  deriving (Show)

data Expr
  = BinOp BinOp Expr Expr
  | UnaOp UnaOp Expr
  | Int Int
  | String String
  | Var String
  | Brack Expr
  | Boolean Bool
  | FuncCall String [Expr]
  deriving (Show)

data Block
  = Curly Stmts
  | Expr Expr
  deriving (Show)

data Type
  = Type [String]
  | Untyped
  deriving (Show)
