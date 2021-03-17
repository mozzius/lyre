module Language.Pretty where

import Language.Syntax

prettys :: Stmts -> String
prettys = concatMap (\x -> pretty x ++ " ")

class Pretty p where
  pretty :: p -> String

instance Pretty Stmt where
  pretty (Let name type' expr) = "LET " ++ name ++ " " ++ pretty type' ++ pretty expr
  pretty (Return expr) = "RETURN " ++ pretty expr
  pretty (FuncDef name args typ block) = "FUNC " ++ name ++ " " ++ unwords (map pretty args) ++ " " ++ pretty typ ++ pretty block
  pretty (If expr block) = "IF " ++ pretty expr ++ pretty block
  pretty (IfElse expr block1 block2) = "IFELSE " ++ pretty expr ++ pretty block1 ++ pretty block2
  pretty (IfElseIf expr block stmt) = "IF " ++ pretty expr ++ pretty stmt
  pretty (Expr expr) = pretty expr

instance Pretty BinOp where
  pretty Or = "OR "
  pretty And = "AND "
  pretty Plus = "PLUS "
  pretty Minus = "MINUS "
  pretty Div = "DIV "
  pretty Times = "TIMES "

instance Pretty UnaOp where
  pretty Inv = "NOT "

instance Pretty OptType where
  pretty (Type types) = ": " ++ pretty types
  pretty NoType = ": NO TYPE "

instance Pretty Type where
  pretty BoolType = "BOOLEAN"
  pretty IntType = "INT"
  pretty StringType = "STRING"
  pretty (FuncType argTypes returnType) = "FUNC (" ++ unwords (map pretty argTypes) ++ ") : " ++ pretty returnType
  pretty (ChannelType type') = "CHANNEL (" ++ pretty type' ++ ")"

instance Pretty Argument where
  pretty (Arg name type') = name ++ " " ++ pretty type'

instance Pretty Expr where
  pretty (BinOp operator exp1 exp2) = "EXPR " ++ pretty operator ++ pretty exp1 ++ pretty exp2
  pretty (UnaOp operator exp) = "EXPR " ++ pretty operator ++ pretty exp
  pretty (Int integer) = show integer ++ " "
  pretty (Var name) = name ++ " "
  pretty (Brack exp) = pretty exp
  pretty (App name exps) =
    "CALL " ++ name ++ " "
      ++ concatMap (\x -> pretty x ++ " ") exps

instance Pretty Block where
  pretty (Curly stmts) = prettys stmts
  pretty (Inline exp) = pretty exp