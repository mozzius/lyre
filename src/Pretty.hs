module Pretty where

import Exprs

prettys :: Stmts -> String
prettys = concatMap (\x -> pretty x ++ " ")

class Pretty p where
  pretty :: p -> String

instance Pretty Stmt where
  pretty (Let name expr) = "LET " ++ name ++ " " ++ pretty expr
  pretty (Return expr) = "RETURN " ++ pretty expr
  pretty (FuncDef name args block) = "FUNC " ++ init name ++ " " ++ unwords args ++ " " ++ pretty block

instance Pretty BinOp where
  pretty Or = "OR "
  pretty And = "AND "
  pretty Plus = "PLUS "
  pretty Minus = "MINUS "
  pretty Div = "DIV "
  pretty Times = "TIMES "

instance Pretty UnaOp where
  pretty Inv = "NOT "

instance Pretty Expr where
  pretty (BinOp operator exp1 exp2) = "EXPR " ++ pretty operator ++ pretty exp1 ++ pretty exp2
  pretty (UnaOp operator exp) = "EXPR " ++ pretty operator ++ pretty exp
  pretty (Int integer) = show integer ++ " "
  pretty (Var name) = name ++ " "
  pretty (Brack exp) = pretty exp
  pretty (FuncCall name exps) = "CALL " ++ init name ++ " "
    ++ concatMap (\x -> pretty x ++ " ") exps

instance Pretty Block where
  pretty (Curly stmts) = prettys stmts
  pretty (Expr exp) = pretty exp