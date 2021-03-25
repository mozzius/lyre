{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.TypeChecker where

import Data.List (intercalate)
import Language.Pretty (pretty)
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

-- ["int", "str", "print", "length", "make", "spawn", "send", "recv"]

type Env = [(String, OptType)]

data BoolWithError
  = Correct
  | Incorrect String String
  deriving (Eq)

expect :: OptType -> OptType -> BoolWithError
expect typ expected =
  if typ == expected
    then Correct
    else Incorrect (pretty typ) (pretty expected)

expectBoth :: BoolWithError -> BoolWithError -> BoolWithError
expectBoth x y =
  case x of
    Correct -> y
    Incorrect _ _ -> x

expectAll :: BoolWithError -> BoolWithError -> BoolWithError -> BoolWithError
expectAll x y z = expectBoth x (expectBoth y z)

prettyExprTypes :: [Expr] -> Env -> String
prettyExprTypes exprs env = intercalate ", " (map (\x -> pretty $ infer x env) exprs)

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
   in case check stmts NoType env of
        Correct -> stmts
        Incorrect exp msg ->
          error
            ( "Type error: Expected "
                ++ exp
                ++ ", got "
                ++ msg
            )

class TypeChecker t0 where
  infer :: t0 -> Env -> OptType
  check :: t0 -> OptType -> Env -> BoolWithError

instance TypeChecker Stmts where
  check [] expected _ = expect NoType expected
  check ((Return expr) : _) expected env = check expr expected env
  check ((Let name exprType expr) : rest) expected env =
    expectBoth (check expr (Type exprType) env) (check rest expected ((name, Type exprType) : env))
  check ((Expr expr) : rest) expected env =
    expectBoth (check expr expected env) (check rest expected env)
  check ((FuncDef name args returnType block) : rest) expected env =
    let funcType = getFuncSignature args returnType
     in let env' = map (\(Arg argName argType) -> (argName, Type argType)) args ++ ((name, funcType) : env)
         in expectBoth (check block returnType env') (check rest expected env')
  check ((If expr block) : rest) expected env =
    expectAll
      (check expr (Type BoolType) env)
      (check rest expected env)
      ( case block of
          (Curly stmts) -> check (stmts ++ rest) expected env
          (Inline expr) -> check (Expr expr : rest) expected env
      )
  check ((IfElse expr block1 block2) : rest) expected env =
    expectAll
      (check expr (Type BoolType) env)
      ( case block1 of
          (Curly stmts) -> check (stmts ++ rest) expected env
          (Inline expr) -> check (Expr expr : rest) expected env
      )
      ( case block2 of
          (Curly stmts) -> check (stmts ++ rest) expected env
          (Inline expr) -> check (Expr expr : rest) expected env
      )
  check ((IfElseIf expr block stmt) : rest) expected env =
    expectAll
      (check expr (Type BoolType) env)
      (check (stmt : rest) expected env)
      ( case block of
          (Curly stmts) -> check (stmts ++ rest) expected env
          (Inline expr) -> check (Expr expr : rest) expected env
      )

  infer _ _ = NoType

instance TypeChecker Block where
  check (Inline expr) expected env = check [Expr expr] expected env
  check (Curly stmt) expected env = check stmt expected env
  infer (Inline expr) env = infer expr env
  infer _ _ = NoType

instance TypeChecker Expr where
  check (Int _) expected _ = expect (Type IntType) expected
  check (String _) expected _ = expect (Type StringType) expected
  check (Boolean _) expected _ = expect (Type BoolType) expected
  check (Brack expr) expected env = check expr expected env
  check (UnaOp op expr) expected env =
    expectBoth
      (check op expected env)
      (check expr expected env)
  check (BinOp op expr1 expr2) expected env =
    let exprType = infer expr1 env
     in expectBoth
          (check op exprType env)
          (check expr2 exprType env)
  check (Var name) expected env = case lookup name env of
    (Just type') -> expect type' expected
    Nothing -> error ("Type error: \"" ++ name ++ "\" is undefined")
  check (App name exprs) expected env = case name of
    "str" ->
      if length exprs == 1
        then expect (Type StringType) expected
        else error "Type error: Invalid number of arguments given to \"str\""
    "int" ->
      if length exprs == 1
        then expect (Type IntType) expected
        else error "Type error: Invalid number of arguments given to \"int\""
    "length" ->
      if map (`infer` env) exprs == [Type StringType]
        then expect (Type IntType) expected
        else error ("Type error: Invalid arguments given to \"length\" - " ++ prettyExprTypes exprs env)
    "print" ->
      if length exprs == 1
        then expect NoType expected
        else error "Type error: Invalid number of arguments given to \"print\""
    "spawn" ->
      case exprs of
        [] -> error "Type error: \"spawn\" must be given at least one argument"
        (first : rest) ->
          case infer first env of
            (Type (FuncType argTypes NoType)) ->
              if map (`infer` env) rest == map Type argTypes
                then expect NoType expected
                else error ("Type error: Invalid arguments given to \"spawn\" - " ++ prettyExprTypes exprs env)
            _ -> error ("Type error: Invalid arguments given to \"spawn\" - " ++ prettyExprTypes exprs env)
    "send" ->
      case exprs of
        [first, second] -> case infer first env of
          (Type (ChannelType chanType)) ->
            expectBoth
              (expect (infer second env) (Type chanType))
              (expect NoType expected)
          _ -> Incorrect "channel" (prettyExprTypes exprs env)
        _ -> Incorrect "channel" (prettyExprTypes exprs env)
    "recv" -> case exprs of
      [chan] -> case infer chan env of
        (Type (ChannelType chanType)) ->
          expect (Type chanType) expected
        _ -> Incorrect "channel" (prettyExprTypes exprs env)
      _ -> Incorrect "channel" (prettyExprTypes exprs env)
    "make" -> error "Type error: channels created using \"make\" must have their types annotated using \"::\""
    _ ->
      case lookup name env of
        (Just (Type (FuncType argTypes returnType))) ->
          if map (`infer` env) exprs == map Type argTypes
            then expect returnType expected
            else error ("Type error: Invalid arguments given to \"" ++ name ++ "\" - " ++ intercalate ", " (map (\x -> pretty $ infer x env) exprs))
        (Just _) -> error ("Type error: \"" ++ name ++ "\" is not a function")
        Nothing -> error ("Type error: \"" ++ name ++ "\" is undefined")
  check (Enforce (App "make" []) type') expected _ =
    case type' of
      (ChannelType _) -> expect (Type type') expected
      _ -> Incorrect "channel" (pretty type')
  check (Enforce expr type') expected env =
    expectBoth
      (check expr (Type type') env)
      (expect (Type type') expected)
  infer (Int _) _ = Type IntType
  infer (String _) _ = Type StringType
  infer (Boolean _) _ = Type BoolType
  infer (Brack expr) env = infer expr env
  infer (UnaOp op expr) env =
    let exprType = infer expr env
     in case check op exprType env of
          Correct -> exprType
          Incorrect exp msg ->
            error
              ( "Type error: Expected "
                  ++ exp
                  ++ ", got "
                  ++ msg
              )
  infer (BinOp op expr1 expr2) env =
    let exprType = infer expr1 env
     in case expectBoth (check op exprType env) (check expr2 exprType env) of
          Correct -> exprType
          Incorrect exp msg ->
            error
              ( "Type error: Expected "
                  ++ exp
                  ++ ", got "
                  ++ msg
              )
  infer (Var name) env = case lookup name env of
    (Just type') -> type'
    Nothing -> error ("Type error: \"" ++ name ++ "\" is undefined")
  infer (App name exprs) env = case lookup name env of
    (Just (Type (FuncType argTypes returnType))) ->
      if map (`infer` env) exprs == map Type argTypes
        then returnType
        else error ("Type error: Invalid arguments given to \"" ++ name ++ "\" - " ++ intercalate ", " (map (\x -> pretty $ infer x env) exprs))
    (Just _) -> error ("Type error: \"" ++ name ++ "\" is not a function")
    Nothing -> error ("Type error: \"" ++ name ++ "\" is undefined")
  infer (Enforce expr type') env =
    case check expr (Type type') env of
      Correct -> Type type'
      Incorrect exp msg ->
        error
          ( "Type error: Expected "
              ++ exp
              ++ ", got "
              ++ msg
          )

instance TypeChecker UnaOp where
  infer Inv _ = Type BoolType
  check Inv expected _ = expect (Type BoolType) expected

instance TypeChecker BinOp where
  infer Equals _ = error "Cannot infer type of Equals operator"
  infer NotEquals _ = error "Cannot infer type of NotEquals operator"
  infer Or _ = Type BoolType
  infer And _ = Type BoolType
  infer Plus _ = Type IntType
  infer Minus _ = Type IntType
  infer Div _ = Type IntType
  infer Times _ = Type IntType
  infer GreaterThan _ = Type IntType
  infer LessThan _ = Type IntType
  infer GreaterEq _ = Type IntType
  infer LessEq _ = Type IntType
  infer Concat _ = Type StringType
  check Equals expected _ =
    if expected `elem` [Type BoolType, Type StringType, Type IntType]
      then Correct
      else Incorrect (pretty expected) "bool, string, int"
  check NotEquals expected _ =
    if expected `elem` [Type BoolType, Type StringType, Type IntType]
      then Correct
      else Incorrect (pretty expected) "bool, string, int"
  check Or (Type BoolType) _ = Correct
  check And (Type BoolType) _ = Correct
  check Plus (Type IntType) _ = Correct
  check Minus (Type IntType) _ = Correct
  check Div (Type IntType) _ = Correct
  check Times (Type IntType) _ = Correct
  check GreaterThan (Type IntType) _ = Correct
  check LessThan (Type IntType) _ = Correct
  check GreaterEq (Type IntType) _ = Correct
  check LessEq (Type IntType) _ = Correct
  check Concat (Type StringType) _ = Correct
  check op expected _ = Incorrect (pretty expected) (pretty (infer op []))
