{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.TypeChecker where

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

class TypeChecker t r where
  infer :: t -> Env -> r
  check :: t -> r -> Env -> Bool

instance TypeChecker Stmts OptType where
  check [] expectedType _ = expectedType == NoType
  check ((Return expr) : _) expectedType env =
    let exprType = infer expr env
     in expectedType == exprType
  check ((Let name exprType expr) : rest) expectedType env =
    let inferred = infer expr env
     in Type exprType == inferred
          && check rest expectedType ((name, Type exprType) : env)
  check ((Expr expr) : rest) expectedType env =
    let _ = infer expr env :: OptType -- can't think how else to discard value
     in check rest expectedType env
  check ((FuncDef name args returnType block) : rest) expectedType env =
    let funcType = getFuncSignature args returnType
     in let env' = map (\(Arg argName argType) -> (argName, Type argType)) args ++ ((name, funcType) : env)
         in check block returnType env' && check rest expectedType env'
  check ((If expr block) : rest) expectedType env =
    infer expr env == Type BoolType && check rest expectedType env
      && ( case block of
             (Curly stmts) -> check (stmts ++ rest) expectedType env
             (Inline expr) -> check (Expr expr : rest) expectedType env
         )
  check ((IfElse expr block1 block2) : rest) expectedType env =
    infer expr env == Type BoolType
      && ( case block1 of
             (Curly stmts) -> check (stmts ++ rest) expectedType env
             (Inline expr) -> check (Expr expr : rest) expectedType env
         )
      && ( case block2 of
             (Curly stmts) -> check (stmts ++ rest) expectedType env
             (Inline expr) -> check (Expr expr : rest) expectedType env
         )
  check ((IfElseIf expr block stmt) : rest) expectedType env =
    infer expr env == Type BoolType && check (stmt : rest) expectedType env
      && ( case block of
             (Curly stmts) -> check (stmts ++ rest) expectedType env
             (Inline expr) -> check (Expr expr : rest) expectedType env
         )
  infer _ _ = error "Cannot infer statements"

instance TypeChecker Block OptType where
  check (Curly stmt) expectedType env = check stmt expectedType env
  check (Inline expr) expectedType env = check [Expr expr] expectedType env
  infer _ _ = error "Cannot infer blocks"

instance TypeChecker Expr OptType where
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
                      ++ show op
                      ++ " cannot be used on type "
                      ++ show exprType
                  )
  infer (BinOp op expr1 expr2) env =
    let opTypes = infer op env :: [OptType]
     in let expr1Type = infer expr1 env
         in let expr2Type = infer expr2 env
             in if expr1Type == expr2Type && expr1Type `elem` opTypes
                  then expr1Type
                  else
                    error
                      ( "Type mismatch: " ++ show op
                          ++ " cannot be used on "
                          ++ show expr1Type
                          ++ " and "
                          ++ show expr2Type
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
  check _ _ _ = error "Cannot check expressions"

instance TypeChecker UnaOp [OptType] where
  infer Inv _ = [Type BoolType]
  check _ _ _ = error "Cannot check operators"

instance TypeChecker BinOp [OptType] where
  infer Or _ = [Type BoolType]
  infer And _ = [Type BoolType]
  infer Plus _ = [Type IntType]
  infer Minus _ = [Type IntType]
  infer Div _ = [Type IntType]
  infer Times _ = [Type IntType]
  infer Equals _ = [Type BoolType, Type StringType, Type IntType]
  infer NotEquals _ = [Type BoolType, Type StringType, Type IntType]
  infer GreaterThan _ = [Type BoolType]
  infer LessThan _ = [Type BoolType]
  infer GreaterEq _ = [Type BoolType]
  infer LessEq _ = [Type BoolType]
  check _ _ _ = error "Cannot check operators"
