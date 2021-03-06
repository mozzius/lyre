{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Compiler where

import Data.Char (ord)
import qualified Language.CoreErlang.Syntax as Erl
import qualified Language.Syntax as Lyre
import Prelude hiding (exp)

seq :: Erl.Exp -> Erl.Exp -> Erl.Exp
seq x y = x

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

class Compiler source target where
  compile :: source -> target

underscore :: String -> String
underscore str = "_" ++ str

exps :: [Erl.Ann Erl.Exp] -> Erl.Exps
exps x = Erl.Exps (Erl.Constr x)

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

instance Compiler Lyre.Stmts Erl.Exps where
  compile stmts = exps $ compile stmts

instance Compiler [Lyre.Stmt] [Erl.Ann Erl.Exp] where
  compile [] = [Erl.Constr (atom "ok")]
  compile ((Lyre.StrLet name expr) : rest) =
    [Erl.Constr (Erl.Let ([underscore name], exp $ compile expr) (compile rest))]
  compile ((Lyre.StrFuncDef name args block) : rest) =
    [Erl.Constr (Erl.Let ([underscore name], exp (Erl.Lambda (map compile args) (compile block))) (compile rest))]
  compile ((Lyre.If expr block) : rest) =
    Erl.Constr (compile (Lyre.If expr block)) : compile rest
  compile ((Lyre.IfElse expr block elseBlock) : rest) =
    Erl.Constr (compile (Lyre.IfElse expr block elseBlock)) : compile rest
  compile ((Lyre.IfElseIf expr block next) : rest) =
    Erl.Constr (compile (Lyre.IfElseIf expr block next)) : compile rest
  compile ((Lyre.Return expr) : _) = [Erl.Constr (compile expr)]
  compile (Lyre.Let {} : _) = error "Let statement is still typed, please strip types before compilation"
  compile (Lyre.FuncDef {} : _) = error "FuncDef statement is still typed, please strip types before compilation"

instance Compiler Lyre.Stmt Erl.FunDef where
  compile (Lyre.StrFuncDef name args expr) = Erl.FunDef (Erl.Constr (Erl.Function (Erl.Atom name, toInteger $ length args))) (Erl.Constr (Erl.Lambda (map (\(Lyre.StrArg a) -> underscore a) args) (compile expr)))
  compile _ = error "Only (type-stripped) FuncDefs are allowed at the base level of a program"

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
              (exps [Erl.Constr (compile next)])
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
  compile (Lyre.Curly stmts) = compile stmts
  compile (Lyre.Expr expr) = exp $ compile expr

-- fundef = FunDef
--   (Constr (Function (Atom "main", 0)))
--   ( Constr
--     ( Lambda
--       []
--       ( Exp
--         ( Constr
--           ( Let
--             ( ["PID"]
--             , Exp
--               ( Constr
--                 ( ModCall
--                   ( Exp (Constr (Lit (LAtom (Atom "erlang"))))
--                   , Exp (Constr (Lit (LAtom (Atom "spawn"))))
--                   )
--                   [ Exp (Constr (Lit (LAtom (Atom "test"))))
--                   , Exp (Constr (Lit (LAtom (Atom "main"))))
--                   , Exp (Constr (Lit LNil))
--                   ]
--                 )
--               )
--             )
--             ( Exp
--               ( Constr
--                 ( ModCall
--                   ( Exp (Constr (Lit (LAtom (Atom "erlang"))))
--                   , Exp (Constr (Lit (LAtom (Atom "!"))))
--                   )
--                   [ Exp (Constr (Var "PID"))
--                   , Exp (Constr (Lit (LAtom (Atom "hello"))))
--                   ]
--                 )
--               )
--             )
--           )
--         )
--       )
--     )
--   )

-- funcall = FunDef
--   (Constr (Function (Atom "main", 0)))
--   ( Constr
--     ( Lambda
--       []
--       (Exp (Constr (App (Exp (Constr (Fun (Function (Atom "main", 0))))) [])))
--     )
--   )

-- plusone = FunDef
--   (Constr (Function (Atom "main", 1)))
--   ( Constr
--     ( Lambda
--       ["_0"]
--       ( Exp
--         ( Constr
--           ( ModCall
--             ( Exp (Constr (Lit (LAtom (Atom "erlang"))))
--             , Exp (Constr (Lit (LAtom (Atom "+"))))
--             )
--             [Exp (Constr (Var "_0")), Exp (Constr (Lit (LInt 1)))]
--           )
--         )
--       )
--     )
--   )

-- iff =
--   FunDef
--     (Constr (Function (Atom "main", 1)))
--     ( Constr
--         ( Lambda
--             ["_0"]
--             ( Exp
--                 ( Constr
--                     ( Case
--                         (Exps (Constr []))
--                         [ Constr
--                             ( Alt
--                                 (Pats [])
--                                 ( Guard
--                                     ( Exp
--                                         ( Constr
--                                             ( ModCall
--                                                 ( Exp (Constr (Lit (LAtom (Atom "erlang")))),
--                                                   Exp (Constr (Lit (LAtom (Atom "=="))))
--                                                 )
--                                                 [Exp (Constr (Erl.Var "_0")), Exp (Constr (Lit (LInt 1)))]
--                                             )
--                                         )
--                                     )
--                                 )
--                                 (Exp (Constr (Lit (LInt 1))))
--                             ),
--                           Constr
--                             ( Alt
--                                 (Pats [])
--                                 ( Guard
--                                     ( Exp
--                                         ( Constr
--                                             ( ModCall
--                                                 ( Exp (Constr (Lit (LAtom (Atom "erlang")))),
--                                                   Exp (Constr (Lit (LAtom (Atom "=="))))
--                                                 )
--                                                 [Exp (Constr (Erl.Var "_0")), Exp (Constr (Lit (LInt 2)))]
--                                             )
--                                         )
--                                     )
--                                 )
--                                 (Exp (Constr (Lit (LInt 2))))
--                             ),
--                           Constr
--                             ( Alt
--                                 (Pats [])
--                                 (Guard (Exp (Constr (Lit (LAtom (Atom "true"))))))
--                                 (Exp (Constr (Lit (LInt 0))))
--                             )
--                         ]
--                     )
--                 )
--             )
--         )
--     )

-- full = Right (Constr (Module (Atom "test") [Erl.Function (Atom "main", 1), Erl.Function (Atom "module_info", 0), Erl.Function (Atom "module_info", 1), Erl.Function (Atom "tow", 3)] [(Atom "file", CList (L [CTuple [CList (LL [CLit (LInt 115)] (CList (LL [CLit (LInt 114)] (CList (LL [CLit (LInt 99)] (CList (LL [CLit (LInt 47)] (CList (LL [CLit (LInt 116)] (CList (LL [CLit (LInt 101)] (CList (LL [CLit (LInt 115)] (CList (LL [CLit (LInt 116)] (CList (LL [CLit (LInt 46)] (CList (LL [CLit (LInt 101)] (CList (LL [CLit (LInt 114)] (CList (L [CLit (LInt 108)]))))))))))))))))))))))), CLit (LInt 1)]]))] [FunDef (Constr (Erl.Function (Atom "tow", 3))) (Constr (Lambda ["_0", "_1", "_2"] (Exp (Constr (List (LL [Exp (Constr (Var "_0"))] (Exp (Constr (List (LL [Exp (Constr (Var "_1"))] (Exp (Constr (List (LL [Exp (Constr (Var "_2"))] (Exp (Constr (Lit LNil))))))))))))))))), FunDef (Constr (Erl.Function (Atom "main", 1))) (Constr (Lambda ["_0"] (Exp (Constr (Case (Exps (Constr [])) [Constr (Alt (Pats []) (Guard (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))), Exp (Constr (Lit (LAtom (Atom "=="))))) [Exp (Constr (Var "_0")), Exp (Constr (Lit (LInt 1)))])))) (Exp (Constr (Lit (LInt 1))))), Constr (Alt (Pats []) (Guard (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))), Exp (Constr (Lit (LAtom (Atom "=="))))) [Exp (Constr (Var "_0")), Exp (Constr (Lit (LInt 2)))])))) (Exp (Constr (Lit (LInt 2))))), Constr (Alt (Pats []) (Guard (Exp (Constr (Lit (LAtom (Atom "true")))))) (Exp (Constr (Lit (LInt 0)))))]))))), FunDef (Constr (Erl.Function (Atom "module_info", 0))) (Constr (Lambda [] (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))), Exp (Constr (Lit (LAtom (Atom "get_module_info"))))) [Exp (Constr (Lit (LAtom (Atom "test"))))]))))), FunDef (Constr (Erl.Function (Atom "module_info", 1))) (Constr (Lambda ["_0"] (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))), Exp (Constr (Lit (LAtom (Atom "get_module_info"))))) [Exp (Constr (Lit (LAtom (Atom "test")))), Exp (Constr (Var "_0"))])))))]))