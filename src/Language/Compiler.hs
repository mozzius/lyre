{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Compiler where

import Data.Char (ord)
import qualified Language.CoreErlang.Syntax as Erl
import qualified Language.Syntax as Lyre
import Prelude hiding (exp)

data Func = Func Lyre.Stmt String Int

getFunc :: Lyre.Stmt -> Func
getFunc (Lyre.StrFuncDef name args block) = Func (Lyre.StrFuncDef name args block) name (length args)

getFuncs :: Lyre.Stmts -> [Func]
getFuncs =
  map
    ( \case
        (Lyre.StrFuncDef name args block) -> getFunc (Lyre.StrFuncDef name args block)
        _ -> error "Base level of Lyre programs should be FuncDefs (with stripped types)"
    )

compileModule :: String -> Lyre.Stmts -> Erl.Module
compileModule name program = let funcs = getFuncs program in Erl.Module (Erl.Atom name) (map (\(Func _ name arity) -> Erl.Function (Erl.Atom name, toInteger arity)) funcs) [(Erl.Atom "file", constructFileName name)] (map (`compile` []) program)

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

transformName :: String -> [(String, String)] -> String
transformName name env = case lookup name env of
  Just name -> name
  Nothing -> error "Variable is not declared"

addVar :: String -> [(String, String)] -> (String, [(String, String)])
addVar name env = case lookup name env of
  Just name -> error "Name already in use"
  Nothing -> (name, (name, underscore name) : env)

addFunc :: String -> [(String, String)] -> (String, [(String, String)])
addFunc name env = case lookup name env of
  Just name -> error "Name already in use"
  Nothing -> (name, (name, name) : env)

addArgs :: [Lyre.Argument] -> [(String, String)] -> ([String], [(String, String)])
addArgs [] env = ([], env)
addArgs ((Lyre.StrArg arg) : rest) env0 = let (name, env1) = addVar arg env0 in let (names, env2) = addArgs rest env1 in (name : names, env2)

class Compiler source target where
  compile :: source -> [(String, String)] -> target

-- For base level function declarations
instance Compiler Lyre.Stmt Erl.FunDef where
  compile (Lyre.StrFuncDef name args0 block) env0 =
    let (name, env1) = addFunc name env0
     in let (args1, env2) = addArgs args0 env1
         in Erl.FunDef (Erl.Constr (Erl.Function (Erl.Atom name, toInteger $ length args0))) (Erl.Constr (Erl.Lambda args1 (compile block env2)))
  compile _ _ = error "Only (type-stripped) FuncDefs are allowed at the base level of a program"

instance Compiler Lyre.Stmts Erl.Exp where
  compile [] _ = atom "ok"
  compile ((Lyre.StrLet name expr) : rest) env =
    let (name, newEnv) = addVar name env
     in Erl.Let ([name], exp $ compile expr newEnv) (exp $ compile rest newEnv)
  compile ((Lyre.StrFuncDef name args block) : rest) env =
    let (name, newEnv) = addVar name env
     in Erl.Let ([name], exp (Erl.Lambda (map (`compile` newEnv) args) (compile block newEnv))) (exp $ compile rest newEnv)
  compile ((Lyre.If expr block) : rest) env =
    Erl.Seq (exp $ compile (Lyre.If expr block) env) (exp $ compile rest env)
  compile ((Lyre.IfElse expr block elseBlock) : rest) env =
    Erl.Seq (exp $ compile (Lyre.IfElse expr block elseBlock) env) (exp $ compile rest env)
  compile ((Lyre.IfElseIf expr block next) : rest) env =
    Erl.Seq (exp $ compile (Lyre.IfElseIf expr block next) env) (exp $ compile rest env)
  compile ((Lyre.Return expr) : _) env = compile expr env
  compile (Lyre.Let {} : _) env = error "Let statement is still typed, please strip types before compilation"
  compile (Lyre.FuncDef {} : _) env = error "FuncDef statement is still typed, please strip types before compilation"

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

instance Compiler Lyre.Argument String where
  compile (Lyre.StrArg name) env = transformName name env
  compile (Lyre.Arg _ _) _ = error "Arguements are still typed, please strip types before compilation"

instance Compiler Lyre.BinOp String where
  compile Lyre.Or _ = "or"
  compile Lyre.And _ = "and"
  compile Lyre.Plus _ = "+"
  compile Lyre.Minus _ = "-"
  compile Lyre.Div _ = "/"
  compile Lyre.Times _ = "*"

instance Compiler Lyre.UnaOp String where
  compile Lyre.Inv _ = "not"

instance Compiler Lyre.Expr Erl.Exp where
  compile (Lyre.BinOp operator expr1 expr2) env = erlangCall (compile operator env) [exp $ compile expr1 env, exp $ compile expr2 env]
  compile (Lyre.UnaOp operator expr) env = erlangCall (compile operator env) [exp $ compile expr env]
  compile (Lyre.Int integer) env = Erl.Lit (Erl.LInt (toInteger integer))
  compile (Lyre.Var name) env = Erl.Var (transformName name env)
  compile (Lyre.Brack expr) env = compile expr env
  compile (Lyre.App name args) env =
    let arity = toInteger . length $ args
     in Erl.App (Erl.Exp (Erl.Constr (Erl.Fun (Erl.Function (Erl.Atom name, arity))))) (map (\x -> exp $ compile x env) args)
  compile (Lyre.String literal) env = stringToList literal
  compile (Lyre.Boolean boolean) env = if boolean then atom "true" else atom "false"

instance Compiler Lyre.Block Erl.Exps where
  compile (Lyre.Curly stmts) env = exp $ compile stmts env
  compile (Lyre.Expr expr) env = exp $ compile expr env
