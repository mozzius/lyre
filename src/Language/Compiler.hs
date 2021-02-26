{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Compiler where

import Language.Exprs as Lyre
import Language.CoreErlang.Syntax as Erl
import Data.Char (ord)

class Compiler source target where
  compile :: source -> target

underscore :: String -> String
underscore str = "_" ++ str

instance Compiler Lyre.Stmts Erl.Exps where
  compile stmts = Erl.Exps (
    case stmts of
      -- [] -> []
      ((Lyre.Let name expr):rest) ->
        (Erl.Let ([underscore name], (compile expr)) (compile rest))
      ((Lyre.FuncDef name args block):rest) ->
        (Erl.Let ([underscore name], Erl.Exp (Erl.Constr (Erl.Lambda (map underscore args) (compile block)))) (compile rest))
      -- ((Lyre.Return expr):rest) -> (compile expr):(compile rest)
    )

-- instance Compiler Lyre.BinOp Erl.Exps where
--   compile Lyre.Or = "OR "
--   compile Lyre.And = "AND "
--   compile Lyre.Plus = "PLUS "
--   compile Lyre.Minus = "MINUS "
--   compile Lyre.Div = "DIV "
--   compile Lyre.Times = "TIMES "

-- instance Compiler Lyre.UnaOp Erl.Exps where
--   compile Inv = "NOT "

instance Compiler Lyre.Expr Erl.Exps where
  -- compile (Lyre.BinOp operator expr1 expr2)
  -- compile (Lyre.UnaOp operator expr)
  compile (Lyre.Int integer) = Erl.Exp (Erl.Constr(Erl.Lit (Erl.LInt (toInteger integer))))
  compile (Lyre.Var name) = Erl.Exp (Erl.Constr(Erl.Var (underscore name)))
  compile (Lyre.Brack expr) = compile expr
  compile (Lyre.FuncCall name args) = let arity = toInteger . length $ args in Erl.Exp(Erl.Constr(Erl.App(Erl.Exp (Erl.Constr(Erl.Fun(Erl.Function(Erl.Atom name, arity))))) (map compile args)))

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

constructList :: [Erl.Exps] -> Erl.Exps
constructList []  = Erl.Exp (Erl.Constr (Erl.Lit Erl.LNil))
constructList [x] = (Erl.Exp (Erl.Constr (Erl.List (Erl.L [x]))))
constructList (x:xs) =
  (Erl.Exp (Erl.Constr (Erl.List (Erl.LL [x] (constructList xs)))))

stringToList :: String -> Erl.Exps
stringToList str = constructList $ map
  (\x -> Erl.Exp (Erl.Constr (Erl.Lit (Erl.LInt (toInteger $ ord x)))))
  str
