{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Compiler where

import Prelude hiding (exp) 
import Language.Syntax as Lyre
import Language.CoreErlang.Syntax as Erl
import Data.Char (ord)

class Compiler source target where
  compile :: source -> target

underscore :: String -> String
underscore str = "_" ++ str

exps :: [Erl.Ann Erl.Exp] -> Erl.Exps
exps x = Erl.Exps (Erl.Constr x)

exp :: (Erl.Exp) -> Erl.Exps
exp x = Erl.Exp (Erl.Constr x)

atom :: String -> Erl.Exps
atom x = Erl.Exp (Erl.Constr (Erl.Lit (Erl.LAtom (Erl.Atom x))))

moduleCall :: String -> String -> [Erl.Exps] -> Erl.Exps
moduleCall x y args =
  (Erl.Exp (Erl.Constr (Erl.ModCall (atom x, atom y) args)))

erlangCall :: String -> [Erl.Exps] -> Erl.Exps
erlangCall = moduleCall "erlang"

constructList :: [Erl.Exps] -> Erl.Exps
constructList []  = Erl.Exp (Erl.Constr (Erl.Lit Erl.LNil))
constructList [x] = (Erl.Exp (Erl.Constr (Erl.List (Erl.L [x]))))
constructList (x:xs) =
  (Erl.Exp (Erl.Constr (Erl.List (Erl.LL [x] (constructList xs)))))

stringToList :: String -> Erl.Exps
stringToList str = constructList $ map
  (\x -> Erl.Exp (Erl.Constr (Erl.Lit (Erl.LInt (toInteger $ ord x)))))
  str

instance Compiler Lyre.Stmts Erl.Exps where
  compile stmts = Erl.Exps (Erl.Constr (
    case stmts of
      [] -> []
      ((Lyre.StrLet name expr):rest) ->
        [Erl.Constr (Erl.Let ([underscore name], (compile expr)) (compile rest))]
      ((Lyre.StrFuncDef name args block):rest) ->
        [Erl.Constr (Erl.Let ([underscore name], exp (Erl.Lambda (map compile args) (compile block))) (compile rest))]
      ((Lyre.Return expr):_) -> [(Erl.Constr (compile expr))]
      -- TODO If statements
      ((Lyre.Let _ _ _):_) -> error "Let statement is still typed, please strip types before compilation"
      ((Lyre.FuncDef _ _ _ _):_) -> error "FuncDef statement is still typed, please strip types before compilation"
    ))

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

instance Compiler Lyre.Expr Erl.Exps where
  compile (Lyre.BinOp operator expr1 expr2) = erlangCall (compile operator) [compile expr1, compile expr2]
  compile (Lyre.UnaOp operator expr) = erlangCall (compile operator) [compile expr]
  compile (Lyre.Int integer) = Erl.Exp (Erl.Constr(Erl.Lit (Erl.LInt (toInteger integer))))
  compile (Lyre.Var name) = Erl.Exp (Erl.Constr(Erl.Var (underscore name)))
  compile (Lyre.Brack expr) = compile expr
  compile (Lyre.App name args) = let arity = toInteger . length $ args in Erl.Exp(Erl.Constr(Erl.App(Erl.Exp (Erl.Constr(Erl.Fun(Erl.Function(Erl.Atom name, arity))))) (map compile args)))
  compile (Lyre.String literal) = stringToList literal
  compile (Lyre.Boolean boolean) = if boolean then atom "true" else atom "false"

instance Compiler Lyre.Block Erl.Exps where
  compile (Lyre.Curly stmts) = compile stmts
  compile (Lyre.Expr expr) = compile expr

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