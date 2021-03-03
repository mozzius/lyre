module Language.Pretty where

import Language.Syntax

prettys :: Stmts -> String
prettys = concatMap (\x -> pretty x ++ " ")

class Pretty p where
  pretty :: p -> String

instance Pretty Stmt where
  pretty (Let name typ expr) = "LET " ++ name ++ " " ++ pretty typ ++ pretty expr
  pretty (Return expr) = "RETURN " ++ pretty expr
  pretty (FuncDef name args typ block) = "FUNC " ++ name ++ " " ++ unwords (map pretty args) ++ " " ++ pretty typ ++ pretty block
  pretty (If expr block) = "IF " ++ pretty expr ++ pretty block
  pretty (IfElse expr block1 block2) = "IFELSE " ++ pretty expr ++ pretty block1 ++ pretty block2
  pretty (IfElseIf expr block stmt) = "IF " ++ pretty expr ++ pretty stmt

instance Pretty BinOp where
  pretty Or = "OR "
  pretty And = "AND "
  pretty Plus = "PLUS "
  pretty Minus = "MINUS "
  pretty Div = "DIV "
  pretty Times = "TIMES "

instance Pretty UnaOp where
  pretty Inv = "NOT "

instance Pretty Type where
  pretty (Type types) = ": " ++ unwords types
  pretty Untyped = ": UNTYPED "

instance Pretty Argument where
  pretty (Arg name typ) = name ++ " " ++ pretty typ

instance Pretty Expr where
  pretty (BinOp operator exp1 exp2) = "EXPR " ++ pretty operator ++ pretty exp1 ++ pretty exp2
  pretty (UnaOp operator exp) = "EXPR " ++ pretty operator ++ pretty exp
  pretty (Int integer) = show integer ++ " "
  pretty (Var name) = name ++ " "
  pretty (Brack exp) = pretty exp
  pretty (FuncCall name exps) = "CALL " ++ name ++ " "
    ++ concatMap (\x -> pretty x ++ " ") exps

instance Pretty Block where
  pretty (Curly stmts) = prettys stmts
  pretty (Expr exp) = pretty exp