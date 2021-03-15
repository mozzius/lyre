{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.TypeChecker where

import Language.Syntax

type Env = [(String, Type)]

getEnv :: Stmts -> Env
getEnv =
  map
    ( \(FuncDef name _ ftype _) -> case ftype of
        (Type types) -> (name, Type ("func" : types))
        Untyped -> (name, Type ["func"])
    )

typeCheck :: Stmts -> Stmts
typeCheck stmts =
  let env = getEnv stmts
   in let (_, stmts') = check stmts env
       in stmts'

class TypeChecker t where
  check :: t -> Env -> (Type, t)

instance TypeChecker Stmts where
  check ((Return expr) : _) env =
    let (exprType, expr') = check expr env
     in ()
  check ((Let name ltype expr) : rest) env =
    let (exprType, expr') = check expr env
     in if exprType == ltype
          then check rest ((name, ltype) : env)
          else error "Type error"

instance TypeChecker Stmt where
  check (Let name type' expr) env = StrLet name expr
  check (FuncDef name args _ block) env = StrFuncDef name (map check args) (check block)
  check (If expr block) env = If expr (check block)
  check (IfElse expr block1 block2) env = IfElse expr (check block1) (check block2)
  check (IfElseIf expr block stmt) env = IfElseIf expr (check block) (check stmt)
  check rest env = rest

instance TypeChecker Argument where
  check (Arg name _) env = StrArg name
  check rest env = rest

instance TypeChecker Block where
  check (Curly stmts) env = Curly (check stmts)
  check rest env = rest
