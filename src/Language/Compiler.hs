{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Compiler where

import Data.Char (ord)
import Language.CoreErlang.Syntax as Erl
import Language.Syntax as Lyre
import Prelude hiding (exp)

class Compiler source target where
  compile :: source -> target

underscore :: String -> String
underscore str = "_" ++ str

exps :: [Erl.Ann Erl.Exp] -> Erl.Exps
exps x = Erl.Exps (Erl.Constr x)

exp :: (Erl.Exp) -> Erl.Exps
exp x = Erl.Exp (Erl.Constr x)

atom :: String -> Erl.Exp
atom x = Erl.Lit (Erl.LAtom (Erl.Atom x))

expAtom :: String -> Erl.Exps
expAtom x = exp $ atom x

moduleCall :: String -> String -> [Erl.Exps] -> Erl.Exp
moduleCall x y args = (Erl.ModCall (expAtom x, expAtom y) args)

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

instance Compiler Lyre.Stmts Erl.Exps where
  compile stmts =
    Erl.Exps
      ( Erl.Constr
          ( case stmts of
              [] -> []
              ((Lyre.StrLet name expr) : rest) ->
                [Erl.Constr (Erl.Let ([underscore name], exp $ compile expr) (compile rest))]
              ((Lyre.StrFuncDef name args block) : rest) ->
                [Erl.Constr (Erl.Let ([underscore name], exp (Erl.Lambda (map compile args) (compile block))) (compile rest))]
              ((Lyre.If expr block) : rest) ->
                [ Erl.Constr
                    ( Erl.Case
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
                    )
                ]
              ((Lyre.IfElse expr block elseBlock) : rest) ->
                [ Erl.Constr
                    ( Erl.Case
                        (Erl.Exps (Erl.Constr []))
                        [ Erl.Constr
                            ( Erl.Alt
                                (Erl.Pats [])
                                ( Erl.Guard
                                    (exp $ compile expr)
                                )
                                (exps $ compile block)
                            ),
                          Erl.Constr
                            ( Erl.Alt
                                (Erl.Pats [])
                                (Erl.Guard (expAtom "true"))
                                (exps $ compile elseBlock)
                            )
                        ]
                    )
                ]
              ((Lyre.IfElseIf expr block next) : rest) ->
                [ Erl.Constr
                    ( Erl.Case
                        (Erl.Exps (Erl.Constr []))
                        [ Erl.Constr
                            ( Erl.Alt
                                (Erl.Pats [])
                                ( Erl.Guard
                                    (exp $ compile expr)
                                )
                                (exps $ compile block)
                            ),
                          Erl.Constr
                            ( Erl.Alt
                                (Erl.Pats [])
                                (Erl.Guard (expAtom "true"))
                                (compile next)
                            )
                        ]
                    )
                ]
              -- ((Lyre.IfElseIf expr block nextIf):rest) ->
              --   [Erl.Constr (Erl.Case (Erl.Exps (Erl.Constr [])) [
              --     (Erl.Constr (Erl.Alt (Erl.Pats [])
              --                          (Erl.Guard (exp $ compile expr)
              --                          (exps $ compile block)),
              --     (Erl.Constr (Erl.Alt (Erl.Pats [])
              --                          (Erl.Guard (expAtom "true"))
              --                          (exps $ compile [nextIf])))
              --   ))])]
              ((Lyre.Return expr) : _) -> [Erl.Constr (compile expr)]
              (Lyre.Let {} : _) -> error "Let statement is still typed, please strip types before compilation"
              (Lyre.FuncDef {} : _) -> error "FuncDef statement is still typed, please strip types before compilation"
          )
      )


instance Compiler Lyre.Stmts Erl.Exps where
  compile (Lyre.If) =

instance Compiler Lyre.Argument String where
  compile (StrArg name) = underscore name
  compile (Arg _ _) = error "Arguements are still typed, please strip types before compilation"

instance Compiler Lyre.BinOp String where
  compile Lyre.Or = "or"
  compile Lyre.And = "and"
  compile Lyre.Plus = "+"
  compile Lyre.Minus = "-"
  compile Lyre.Div = "/"
  compile Lyre.Times = "*"

instance Compiler Lyre.UnaOp String where
  compile Inv = "not"

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

iff =
  FunDef
    (Constr (Function (Atom "main", 1)))
    ( Constr
        ( Lambda
            ["_0"]
            ( Exp
                ( Constr
                    ( Case
                        (Exps (Constr []))
                        [ Constr
                            ( Alt
                                (Pats [])
                                ( Guard
                                    ( Exp
                                        ( Constr
                                            ( ModCall
                                                ( Exp (Constr (Lit (LAtom (Atom "erlang")))),
                                                  Exp (Constr (Lit (LAtom (Atom "=="))))
                                                )
                                                [Exp (Constr (Erl.Var "_0")), Exp (Constr (Lit (LInt 1)))]
                                            )
                                        )
                                    )
                                )
                                (Exp (Constr (Lit (LInt 1))))
                            ),
                          Constr
                            ( Alt
                                (Pats [])
                                ( Guard
                                    ( Exp
                                        ( Constr
                                            ( ModCall
                                                ( Exp (Constr (Lit (LAtom (Atom "erlang")))),
                                                  Exp (Constr (Lit (LAtom (Atom "=="))))
                                                )
                                                [Exp (Constr (Erl.Var "_0")), Exp (Constr (Lit (LInt 2)))]
                                            )
                                        )
                                    )
                                )
                                (Exp (Constr (Lit (LInt 2))))
                            ),
                          Constr
                            ( Alt
                                (Pats [])
                                (Guard (Exp (Constr (Lit (LAtom (Atom "true"))))))
                                (Exp (Constr (Lit (LInt 0))))
                            )
                        ]
                    )
                )
            )
        )
    )
