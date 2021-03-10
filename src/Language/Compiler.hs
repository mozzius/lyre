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
compileModule name program = let funcs = getFuncs program in Erl.Module (Erl.Atom name) (map (\(Func _ name arity) -> Erl.Function (Erl.Atom name, toInteger arity)) funcs) [(Erl.Atom "file", constructFileName name)] (map compile program)

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

class Compiler source target where
  compile :: source -> env -> target

-- For base level function declarations
instance Compiler Lyre.Stmt Erl.FunDef where
  compile (Lyre.StrFuncDef name args block) = Erl.FunDef (Erl.Constr (Erl.Function (Erl.Atom name, toInteger $ length args))) (Erl.Constr (Erl.Lambda (map (\(Lyre.StrArg a) -> underscore a) args) (compile block)))
  compile _ = error "Only (type-stripped) FuncDefs are allowed at the base level of a program"

instance Compiler Lyre.Stmts Erl.Exp where
  compile [] _ = atom "ok"
  compile ((Lyre.StrLet name expr) : rest) env =
    Erl.Let ([lookup name env], exp $ compile expr env) (exp $ compile rest env)
  compile ((Lyre.StrFuncDef name args block) : rest) env =
    Erl.Let ([lookup name env], exp (Erl.Lambda (map compile args env) (compile block env))) (exp $ compile rest env)
  compile ((Lyre.If expr block) : rest) env =
    Erl.Seq (exp $ compile (Lyre.If expr block)) (exp $ compile rest env)
  compile ((Lyre.IfElse expr block elseBlock) : rest) env =
    Erl.Seq (exp $ compile (Lyre.IfElse expr block elseBlock) env) (exp $ compile rest env)
  compile ((Lyre.IfElseIf expr block next) : rest) env =
    Erl.Seq (exp $ compile (Lyre.IfElseIf expr block next)) (exp $ compile rest env)
  compile ((Lyre.Return expr) : _) env = compile expr env
  compile (Lyre.Let {} : _) env = error "Let statement is still typed, please strip types before compilation"
  compile (Lyre.FuncDef {} : _) env = error "FuncDef statement is still typed, please strip types before compilation"

instance Compiler Lyre.Stmt Erl.Exp where
  compile (Lyre.If expr block) =
    Erl.Case
      (Erl.Exps (Erl.Constr []))
      [ Erl.Constr
          ( Erl.Alt
              (Erl.Pats [])
              ( Erl.Guard
                  (exp $ compile expr)
              )
              (compile block)
          )
      ]
  compile (Lyre.IfElse expr block elseBlock) =
    Erl.Case
      (Erl.Exps (Erl.Constr []))
      [ Erl.Constr
          ( Erl.Alt
              (Erl.Pats [])
              ( Erl.Guard
                  (exp $ compile expr)
              )
              (compile block)
          ),
        Erl.Constr
          ( Erl.Alt
              (Erl.Pats [])
              (Erl.Guard (expAtom "true"))
              (compile elseBlock)
          )
      ]
  compile (Lyre.IfElseIf expr block next) =
    Erl.Case
      (Erl.Exps (Erl.Constr []))
      [ Erl.Constr
          ( Erl.Alt
              (Erl.Pats [])
              ( Erl.Guard
                  (exp $ compile expr)
              )
              (compile block)
          ),
        Erl.Constr
          ( Erl.Alt
              (Erl.Pats [])
              (Erl.Guard (expAtom "true"))
              (exp $ compile next)
          )
      ]

instance Compiler Lyre.Argument String where
  compile (Lyre.StrArg name) = underscore name
  compile (Lyre.Arg _ _) = error "Arguements are still typed, please strip types before compilation"

instance Compiler Lyre.BinOp String where
  compile Lyre.Or = "or"
  compile Lyre.And = "and"
  compile Lyre.Plus = "+"
  compile Lyre.Minus = "-"
  compile Lyre.Div = "/"
  compile Lyre.Times = "*"

instance Compiler Lyre.UnaOp String where
  compile Lyre.Inv = "not"

instance Compiler Lyre.Expr Erl.Exp where
  compile (Lyre.BinOp operator expr1 expr2) = erlangCall (compile operator) [exp $ compile expr1, exp $ compile expr2]
  compile (Lyre.UnaOp operator expr) = erlangCall (compile operator) [exp $ compile expr]
  compile (Lyre.Int integer) = Erl.Lit (Erl.LInt (toInteger integer))
  compile (Lyre.Var name) = Erl.Var (underscore name)
  compile (Lyre.Brack expr) = compile expr
  compile (Lyre.App name args) =
    let arity = toInteger . length $ args
     in Erl.App (Erl.Exp (Erl.Constr (Erl.Fun (Erl.Function (Erl.Atom name, arity))))) (map (exp . compile) args)
  compile (Lyre.String literal) = stringToList literal
  compile (Lyre.Boolean boolean) = if boolean then atom "true" else atom "false"

instance Compiler Lyre.Block Erl.Exps where
  compile (Lyre.Curly stmts) = exp $ compile stmts
  compile (Lyre.Expr expr) = exp $ compile expr
