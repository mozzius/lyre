{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.TypeChecker where

import Language.Syntax

-- currently just strips types

class TypeChecker t where
  verify :: t -> t


instance TypeChecker Stmts where
  verify stmts = map verify stmts

instance TypeChecker Stmt where
  verify (Let name _ expr) = (StrLet name expr)
  verify (FuncDef name args _ block) = (StrFuncDef name (map verify args) (verify block))
  verify (If expr block) = (If expr (verify block))
  verify (IfElse expr block1 block2) = (IfElse expr (verify block1) (verify block2))
  verify (IfElseIf expr block stmt) = (IfElseIf expr (verify block) (verify stmt))
  verify rest = rest

instance TypeChecker Argument where
  verify (Arg name _) = (StrArg name)
  verify rest = rest

instance TypeChecker Block where
  verify (Curly stmts) = (Curly (verify stmts))
  verify rest = rest
