module Language.Syntax where

type Stmts = [Stmt]

data Stmt
  = Let String Type Expr
  | Return Expr
  | FuncDef String [Argument] OptType Block
  | If Expr Block
  | IfElse Expr Block Block
  | IfElseIf Expr Block Stmt
  | Expr Expr
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
  | Concat
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
  | App String [Expr]
  deriving (Show)

data Block
  = Curly Stmts
  | Inline Expr
  deriving (Show)

data OptType
  = Type Type
  | NoType
  deriving (Eq, Show)

data Type
  = BoolType
  | StringType
  | IntType
  | FuncType [Type] OptType
  | ChannelType Type
  deriving (Eq, Show)
