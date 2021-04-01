module Language.Pretty where

import Data.List (intercalate)
import Language.Syntax
  ( Argument (..),
    BinOp (..),
    Block (..),
    Expr (..),
    OptType (..),
    Stmt (..),
    Stmts,
    Type (..),
    UnaOp (..),
  )

prettys :: Stmts -> String
prettys stmts = intercalate "\n" (map pretty stmts)

class Pretty p where
  pretty :: p -> String

instance Pretty Stmt where
  pretty (Let name type' expr) =
    "let" ++ name
      ++ ": "
      ++ pretty type'
      ++ " "
      ++ pretty expr
  pretty (Return expr) = "return " ++ pretty expr
  pretty (FuncDef name args typ block) = "func " ++ name ++ "(" ++ intercalate ", " (map pretty args) ++ " " ++ pretty typ ++ pretty block
  pretty (If expr block) = "if (" ++ pretty expr ++ ")" ++ pretty block
  pretty (IfElse expr block1 block2) = "if (" ++ pretty expr ++ ") " ++ pretty block1 ++ " else " ++ pretty block2
  pretty (IfElseIf expr block stmt) = "if (" ++ pretty expr ++ ") " ++ pretty block ++ " " ++ pretty stmt
  pretty (Expr expr) = pretty expr

instance Pretty BinOp where
  pretty Or = "||"
  pretty And = "&&"
  pretty Plus = "+"
  pretty Minus = "-"
  pretty Div = "/"
  pretty Times = "*"
  pretty Equals = "=="
  pretty NotEquals = "!="
  pretty GreaterThan = ">"
  pretty LessThan = "<"
  pretty GreaterEq = ">="
  pretty LessEq = "<="
  pretty Concat = "~"

instance Pretty UnaOp where
  pretty Inv = "!"

instance Pretty OptType where
  pretty (Type type') = pretty type'
  pretty NoType = "NoType"

instance Pretty Type where
  pretty BoolType = "boolean"
  pretty IntType = "int"
  pretty StringType = "string"
  pretty (FuncType argTypes returnType) =
    "func " ++ intercalate "," (map pretty argTypes)
      ++ ( case returnType of
             NoType -> ""
             _ -> " -> " ++ pretty returnType
         )
  pretty (ChannelType type') = "channel " ++ pretty type'

instance Pretty Argument where
  pretty (Arg name type') = name ++ ":" ++ pretty type'

instance Pretty Expr where
  pretty (BinOp operator exp1 exp2) = pretty exp1 ++ " " ++ pretty operator ++ " " ++ pretty exp2
  pretty (UnaOp operator exp) = pretty operator ++ pretty exp
  pretty (Int integer) = show integer
  pretty (Var name) = name
  pretty (Brack exp) = "(" ++ pretty exp ++ ")"
  pretty (App name exps) =
    pretty name
      ++ "("
      ++ intercalate ", " (map pretty exps)
      ++ ")"

instance Pretty Block where
  pretty (Curly stmts) = "{\n" ++ prettys stmts ++ "\n}"
  pretty (Inline exp) = pretty exp