{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.TypeChecker where

import Debug.Trace
import Language.Pretty (pretty)
import Language.Syntax

type Env = [(String, OptType)]

getEnv :: Stmts -> Env
getEnv =
  map
    ( \(FuncDef name args ftype _) ->
        (name, getFuncSignature args ftype)
    )

getFuncSignature :: [Argument] -> OptType -> OptType
getFuncSignature args return = Type (FuncType (map (\(Arg _ argType) -> argType) args) return)

typeCheck :: Stmts -> Stmts
typeCheck stmts =
  let env = getEnv stmts
   in if check stmts NoType env
        then stmts
        else error "Type check failed"

class TypeChecker t0 t1 where
  infer :: t0 -> Env -> t1
  check :: t0 -> t1 -> Env -> Bool

instance TypeChecker Stmts OptType where
  check [] expected _ = expected == NoType
  check ((Return expr) : _) expected env =
    let exprType = infer expr env
     in expected == exprType
  check ((Let name exprType expr) : rest) expected env =
    let inferred = infer expr env
     in Type exprType == inferred
          && check rest expected ((name, Type exprType) : env)
  check ((Expr expr) : rest) expected env = check expr expected env && check rest expected env
  check ((FuncDef name args returnType block) : rest) expected env =
    let funcType = getFuncSignature args returnType
     in let env' = map (\(Arg argName argType) -> (argName, Type argType)) args ++ ((name, funcType) : env)
         in check block returnType env' && check rest expected env'
  check ((If expr block) : rest) expected env = trace "if"
    infer expr env == Type BoolType && check rest expected env
      && ( case block of
             (Curly stmts) -> check (stmts ++ rest) expected env
             (Inline expr) -> check (Expr expr : rest) expected env
         )
  check ((IfElse expr block1 block2) : rest) expected env =
    infer expr env == Type BoolType
      && ( case block1 of
             (Curly stmts) -> check (stmts ++ rest) expected env
             (Inline expr) -> check (Expr expr : rest) expected env
         )
      && ( case block2 of
             (Curly stmts) -> check (stmts ++ rest) expected env
             (Inline expr) -> check (Expr expr : rest) expected env
         )
  check ((IfElseIf expr block stmt) : rest) expected env =
    infer expr env == Type BoolType && check (stmt : rest) expected env
      && ( case block of
             (Curly stmts) -> check (stmts ++ rest) expected env
             (Inline expr) -> check (Expr expr : rest) expected env
         )
  infer _ _ = NoType

instance TypeChecker Block OptType where
  check (Inline expr) expected env = check [Expr expr] expected env
  check (Curly stmt) expected env = check stmt expected env
  infer (Inline expr) env = infer expr env
  infer _ _ = NoType

instance TypeChecker Expr OptType where
  check (Int _) expected _ = expected == Type IntType
  check (String _) expected _ = expected == Type StringType
  check (Boolean _) expected _ = expected == Type BoolType
  check (Brack expr) expected env = check expr expected env
  check (UnaOp op expr) expected env =
    let opTypes = infer op env :: [OptType]
     in let exprType = infer expr env
         in if exprType `elem` opTypes
              then expected == exprType
              else
                error
                  ( "Type mismatch: "
                      ++ pretty op
                      ++ " cannot be used on type "
                      ++ pretty exprType
                  )
  check (BinOp op expr1 expr2) expected env =
    let opTypes = infer op env :: [OptType]
     in let expr1Type = infer expr1 env
         in let expr2Type = infer expr2 env
             in if expr1Type == expr2Type && expr1Type `elem` opTypes
                  then expected == expr1Type
                  else
                    error
                      ( "Type mismatch: " ++ pretty op
                          ++ " cannot be used on "
                          ++ pretty expr1Type
                          ++ " and "
                          ++ pretty expr2Type
                      )
  check (Var name) expected env = case lookup name env of
    (Just type') -> expected == type'
    Nothing -> error ("Type error: \"" ++ name ++ "\" is undefined")
  check (App name exprs) expected env = case lookup name env of
    (Just (Type (FuncType argTypes returnType))) ->
      if map (`infer` env) exprs == map Type argTypes
        then expected == returnType
        else error ("Type error: Invalid arguments given to \"" ++ name ++ "\" - " ++ show (map (`infer` env) exprs :: [OptType]))
    (Just _) -> error ("Type error: \"" ++ name ++ "\" is not a function")
    Nothing -> error ("Type error: \"" ++ name ++ "\" is undefined")
  check (Enforce expr type') expected env =
    if check expr (Type type') env
      then expected == Type type'
      else error ("Type error: " ++ pretty expr ++ " is meant to be of type " ++ pretty type' ++ ", but it is of type " ++ pretty (infer expr env :: OptType))
  infer (Int _) _ = Type IntType
  infer (String _) _ = Type StringType
  infer (Boolean _) _ = Type BoolType
  infer (Brack expr) env = infer expr env
  infer (UnaOp op expr) env =
    let opTypes = infer op env :: [OptType]
     in let exprType = infer expr env
         in if exprType `elem` opTypes
              then exprType
              else
                error
                  ( "Type mismatch: "
                      ++ pretty op
                      ++ " cannot be used on type "
                      ++ pretty exprType
                  )
  infer (BinOp op expr1 expr2) env =
    let opTypes = infer op env :: [OptType]
     in let expr1Type = infer expr1 env
         in let expr2Type = infer expr2 env
             in if expr1Type == expr2Type && expr1Type `elem` opTypes
                  then expr1Type
                  else
                    error
                      ( "Type mismatch: " ++ pretty op
                          ++ " cannot be used on "
                          ++ pretty expr1Type
                          ++ " and "
                          ++ pretty expr2Type
                      )
  infer (Var name) env = case lookup name env of
    (Just type') -> type'
    Nothing -> error ("Type error: \"" ++ name ++ "\" is undefined")
  infer (App name exprs) env = case lookup name env of
    (Just (Type (FuncType argTypes returnType))) ->
      if map (`infer` env) exprs == map Type argTypes
        then returnType
        else error ("Type error: Invalid arguments given to \"" ++ name ++ "\" - " ++ show (map (`infer` env) exprs :: [OptType]))
    (Just _) -> error ("Type error: \"" ++ name ++ "\" is not a function")
    Nothing -> error ("Type error: \"" ++ name ++ "\" is undefined")
  infer (Enforce expr type') env =
    if check expr (Type type') env
      then Type type'
      else error ("Type error: " ++ pretty expr ++ " is meant to be of type " ++ pretty type' ++ ", but it is of type " ++ pretty (infer expr env :: OptType))

instance TypeChecker UnaOp [OptType] where
  infer Inv _ = [Type BoolType]
  check _ _ _ = error "Cannot check operators"

instance TypeChecker BinOp [OptType] where
  infer Equals _ = [Type BoolType, Type StringType, Type IntType]
  infer NotEquals _ = [Type BoolType, Type StringType, Type IntType]
  infer Or _ = [Type BoolType]
  infer And _ = [Type BoolType]
  infer Plus _ = [Type IntType]
  infer Minus _ = [Type IntType]
  infer Div _ = [Type IntType]
  infer Times _ = [Type IntType]
  infer GreaterThan _ = [Type IntType]
  infer LessThan _ = [Type IntType]
  infer GreaterEq _ = [Type IntType]
  infer LessEq _ = [Type IntType]
  infer Concat _ = [Type StringType]
  check _ _ _ = error "Cannot check operators"
