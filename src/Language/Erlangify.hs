module Language.Erlangify where

-- import Exprs as L
-- import Language.CoreErlang as E

-- -- Main function that takes the parsed program and turns it into erlang
-- erlangify :: L.Stmts -> String
-- erlangify e = E.pretty $ erlify e

-- erlifys = concatMap erlify

-- class Erlify e where
--   erlify :: e -> erlang

-- instance Erlify L.Stmt where
--   erlify (L.Let name expr) = "LET " ++ name ++ " " ++ erlify expr
--   erlify (L.Return expr) = "RETURN " ++ erlify expr
--   erlify (L.FuncDef name args block) = "FUNC " ++ init name ++ " " ++ unwords args ++ " " ++ erlify block

-- instance Erlify L.BinOp where
--   erlify L.Or = "OR "
--   erlify L.And = "AND "
--   erlify L.Plus = "PLUS "
--   erlify L.Minus = "MINUS "
--   erlify L.Div = "DIV "
--   erlify L.Times = "TIMES "

-- instance Erlify L.UnaOp where
--   erlify Inv = "NOT "

-- instance Erlify L.Expr where
--   erlify (L.BinOp operator exp1 exp2) = "EXPR " ++ erlify operator ++ erlify exp1 ++ erlify exp2
--   erlify (L.UnaOp operator exp) = "EXPR " ++ erlify operator ++ erlify exp
--   erlify (L.Int integer) = show integer ++ " "
--   erlify (L.Var name) = name ++ " "
--   erlify (L.Brack exp) = erlify exp
--   erlify (L.FuncCall name exps) =
--     "CALL " ++ init name ++ " "
--       ++ concatMap (\x -> erlify x ++ " ") exps

-- instance Erlify L.Block where
--   erlify (L.Curly stmts) = erlifys stmts
--   erlify (L.Expr exp) = erlify exp