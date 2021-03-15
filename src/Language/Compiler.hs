{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Language.Compiler where

import Data.Char (ord)
-- import Debug.Trace (trace)
import qualified Language.CoreErlang.Syntax as Erl
import qualified Language.Syntax as Lyre
import Prelude hiding (exp)

data Func = Func Lyre.Stmt String Int

type Env = ([(String, Int)], [(String, String)])

getFunc :: Lyre.Stmt -> Func
getFunc (Lyre.StrFuncDef name args block) = Func (Lyre.StrFuncDef name args block) name (length args)
getFunc _ = error "Statement was not StrFuncDef"

getFuncs :: Lyre.Stmts -> [Func]
getFuncs =
  map
    ( \case
        (Lyre.StrFuncDef name args block) -> getFunc (Lyre.StrFuncDef name args block)
        _ -> error "Base level of Lyre programs should be FuncDefs (with stripped types)"
    )

compileModule :: String -> Lyre.Stmts -> Erl.Module
compileModule name program =
  let funcs = getFuncs program
   in Erl.Module
        (Erl.Atom name)
        (map (\(Func _ fname arity) -> Erl.Function (Erl.Atom fname, toInteger arity)) funcs)
        [(Erl.Atom "file", constructFileName name)]
        (map (`compile` (map (\(Func _ fname arity) -> (fname, arity)) funcs, [])) program)

underscore :: String -> String
underscore str = "_" ++ str

exp :: Erl.Exp -> Erl.Exps
exp x = Erl.Exp (Erl.Constr x)

atom :: String -> Erl.Exp
atom x = Erl.Lit (Erl.LAtom (Erl.Atom x))

expAtom :: String -> Erl.Exps
expAtom x = exp $ atom x

moduleCall :: String -> String -> [Erl.Exps] -> Erl.Exp
moduleCall x y = Erl.ModCall (expAtom x, expAtom y)

erlangCall :: String -> [Erl.Exps] -> Erl.Exp
erlangCall = moduleCall "erlang"

constructList :: [Erl.Exps] -> Erl.Exp
constructList [] = Erl.Lit Erl.LNil
constructList [x] = Erl.List (Erl.L [x])
constructList (x : xs) = Erl.List (Erl.LL [x] (exp $ constructList xs))

stringToList :: String -> Erl.Exp
stringToList str =
  constructList $
    map
      (\x -> Erl.Exp (Erl.Constr (Erl.Lit (Erl.LInt (toInteger $ ord x)))))
      str

constructCList :: [Erl.Const] -> Erl.Const
constructCList [] = Erl.CLit Erl.LNil
constructCList [x] = Erl.CList (Erl.L [x])
constructCList (x : xs) = Erl.CList (Erl.LL [x] (constructCList xs))

constructFileName :: String -> Erl.Const
constructFileName str =
  Erl.CList
    ( Erl.L
        [ Erl.CTuple
            [ constructCList $
                map (\x -> Erl.CLit (Erl.LInt (toInteger $ ord x))) str
            ]
        ]
    )

transformName :: String -> Env -> Either (Erl.Exp, Int) Erl.Exp
transformName name (funcs, vars) = case lookup name funcs of
  Just arity -> Left (Erl.Fun (Erl.Function (Erl.Atom name, toInteger arity)), arity)
  Nothing -> case lookup name vars of
    Just newName -> Right (Erl.Var newName)
    Nothing -> error ("Function \"" ++ name ++ "\" not found")

addVar :: String -> Env -> (String, Env)
addVar name (funcs, vars) = case lookup name funcs of
  Just _ -> error ("Name \"" ++ name ++ "\" already in use")
  Nothing -> case lookup name vars of
    Just _ -> error ("Name \"" ++ name ++ "\" already in use")
    Nothing -> (underscore name, (funcs, (name, underscore name) : vars))

addArgs :: [Lyre.Argument] -> Env -> ([String], Env)
addArgs [] env = ([], env)
addArgs ((Lyre.StrArg arg) : rest) env =
  let (name, env') = addVar arg env
   in let (names, env'') = addArgs rest env'
       in (name : names, env'')
addArgs ((Lyre.Arg _ _) : _) _ = error "Argument was not stripped of types"

class Compiler source target where
  compile :: source -> Env -> target

-- For base level function declarations
instance Compiler Lyre.Stmt Erl.FunDef where
  compile (Lyre.StrFuncDef name args0 block) env =
    let (args1, env') = addArgs args0 env
     in Erl.FunDef
          (Erl.Constr (Erl.Function (Erl.Atom name, toInteger $ length args0)))
          (Erl.Constr (Erl.Lambda args1 (compile block env')))
  compile _ _ = error "Only (type-stripped) FuncDefs are allowed at the base level of a program"

instance Compiler Lyre.Stmts Erl.Exp where
  compile [] _ = atom "ok"
  compile ((Lyre.StrLet name0 expr) : rest) env =
    let (name1, newEnv) = addVar name0 env
     in Erl.Let ([name1], exp $ compile expr newEnv) (exp $ compile rest newEnv)
  compile ((Lyre.StrFuncDef name0 args0 block) : rest) env =
    let (name1, env') = addVar name0 env
     in let (args1, env'') = addArgs args0 env'
         in Erl.Let ([name1], exp (Erl.Lambda args1 (compile block env''))) (exp $ compile rest env')
  compile ((Lyre.If expr block) : rest) env =
    Erl.Seq (exp $ compile (Lyre.If expr block) env) (exp $ compile rest env)
  compile ((Lyre.IfElse expr block elseBlock) : rest) env =
    Erl.Seq (exp $ compile (Lyre.IfElse expr block elseBlock) env) (exp $ compile rest env)
  compile ((Lyre.IfElseIf expr block next) : rest) env =
    Erl.Seq (exp $ compile (Lyre.IfElseIf expr block next) env) (exp $ compile rest env)
  compile ((Lyre.Return expr) : _) env = compile expr env
  compile (Lyre.Let {} : _) _ = error "Let statement is still typed, please strip types before compilation"
  compile (Lyre.FuncDef {} : _) _ = error "FuncDef statement is still typed, please strip types before compilation"

instance Compiler Lyre.Stmt Erl.Exp where
  compile (Lyre.If expr block) env =
    Erl.Case
      (Erl.Exps (Erl.Constr []))
      [ Erl.Constr
          ( Erl.Alt
              (Erl.Pats [])
              ( Erl.Guard
                  (exp $ compile expr env)
              )
              (compile block env)
          )
      ]
  compile (Lyre.IfElse expr block elseBlock) env =
    Erl.Case
      (Erl.Exps (Erl.Constr []))
      [ Erl.Constr
          ( Erl.Alt
              (Erl.Pats [])
              ( Erl.Guard
                  (exp $ compile expr env)
              )
              (compile block env)
          ),
        Erl.Constr
          ( Erl.Alt
              (Erl.Pats [])
              (Erl.Guard (expAtom "true"))
              (compile elseBlock env)
          )
      ]
  compile (Lyre.IfElseIf expr block next) env =
    Erl.Case
      (Erl.Exps (Erl.Constr []))
      [ Erl.Constr
          ( Erl.Alt
              (Erl.Pats [])
              ( Erl.Guard
                  (exp $ compile expr env)
              )
              (compile block env)
          ),
        Erl.Constr
          ( Erl.Alt
              (Erl.Pats [])
              (Erl.Guard (expAtom "true"))
              (exp $ compile next env)
          )
      ]
  compile _ _ = error "Not sure how you ended up here, this is for if statements"

instance Compiler Lyre.BinOp String where
  compile Lyre.Or _ = "or"
  compile Lyre.And _ = "and"
  compile Lyre.Plus _ = "+"
  compile Lyre.Minus _ = "-"
  compile Lyre.Div _ = "/"
  compile Lyre.Times _ = "*"
  compile Lyre.Equals _ = "=="
  compile Lyre.GreaterThan _ = ">"
  compile Lyre.LessThan _ = "<"
  compile Lyre.GreaterEq _ = ">="
  compile Lyre.LessEq _ = "<="
  compile Lyre.NotEquals _ = "/="

instance Compiler Lyre.UnaOp String where
  compile Lyre.Inv _ = "not"

instance Compiler Lyre.Expr Erl.Exp where
  compile (Lyre.BinOp operator expr1 expr2) env =
    erlangCall
      (compile operator env)
      [exp $ compile expr1 env, exp $ compile expr2 env]
  compile (Lyre.UnaOp operator expr) env =
    erlangCall
      (compile operator env)
      [exp $ compile expr env]
  compile (Lyre.Int integer) _ = Erl.Lit (Erl.LInt (toInteger integer))
  compile (Lyre.Var name) env =
    case transformName name env of
      Left (func, _) -> func
      Right var -> var
  compile (Lyre.Brack expr) env = compile expr env
  compile (Lyre.App name args) env =
    let argLen = length args
     in Erl.App
          ( exp
              ( case transformName name env of
                  Left (func, arity) ->
                    if argLen == arity
                      then func
                      else error "Function arity does not match"
                  Right var -> var
              )
          )
          (map (\x -> exp $ compile x env) args)
  compile (Lyre.String literal) _ = stringToList literal
  compile (Lyre.Boolean boolean) _ = if boolean then atom "true" else atom "false"

instance Compiler Lyre.Block Erl.Exps where
  compile (Lyre.Curly stmts) env = exp $ compile stmts env
  compile (Lyre.Expr expr) env = exp $ compile expr env
