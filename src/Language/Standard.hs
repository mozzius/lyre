module Language.Standard where

import Language.CoreErlang.Syntax
  ( Alt (..),
    Ann (..),
    Atom (..),
    Const (..),
    Exp (..),
    Exps (..),
    FunDef (..),
    Function (..),
    Guard (..),
    List (..),
    Literal (..),
    Pat (..),
    Pats (..),
    TimeOut (..),
  )

env :: [(String, Int)]
env =
  [ ("int", 1),
    ("length", 1),
    ("make", 0),
    ("print", 1),
    ("recv", 1),
    ("send", 2),
    ("str", 1)
  ]

header :: [Function]
header =
  [ Function (Atom "int", 1),
    Function (Atom "length", 1),
    Function (Atom "make", 0),
    Function (Atom "make", 1),
    Function (Atom "print", 1),
    Function (Atom "recv", 1),
    Function (Atom "send", 2),
    Function (Atom "str", 1)
  ]

funcs :: String -> [FunDef]
funcs moduleName =
  [ FunDef (Constr (Function (Atom "str", 1))) (Constr (Lambda ["_0"] (Exp (Constr (Case (Exp (Constr (Var "_0"))) [Constr (Alt (Pats [PLit (LAtom (Atom "true"))]) (Guard (Exp (Constr (Lit (LAtom (Atom "true")))))) (Exp (Constr (List (LL [Exp (Constr (Lit (LInt 116)))] (Exp (Constr (List (LL [Exp (Constr (Lit (LInt 114)))] (Exp (Constr (List (LL [Exp (Constr (Lit (LInt 117)))] (Exp (Constr (List (L [Exp (Constr (Lit (LInt 101)))]))))))))))))))))), Constr (Alt (Pats [PLit (LAtom (Atom "false"))]) (Guard (Exp (Constr (Lit (LAtom (Atom "true")))))) (Exp (Constr (List (LL [Exp (Constr (Lit (LInt 102)))] (Exp (Constr (List (LL [Exp (Constr (Lit (LInt 97)))] (Exp (Constr (List (LL [Exp (Constr (Lit (LInt 108)))] (Exp (Constr (List (LL [Exp (Constr (Lit (LInt 115)))] (Exp (Constr (List (L [Exp (Constr (Lit (LInt 101)))]))))))))))))))))))))), Constr (Alt (Pats [PVar "N"]) (Guard (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))), Exp (Constr (Lit (LAtom (Atom "is_integer"))))) [Exp (Constr (Var "_0"))])))) (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))), Exp (Constr (Lit (LAtom (Atom "integer_to_list"))))) [Exp (Constr (Var "N"))])))), Constr (Alt (Pats [PVar "S"]) (Guard (Exp (Constr (Lit (LAtom (Atom "true")))))) (Exp (Constr (Var "S"))))]))))),
    FunDef (Constr (Function (Atom "int", 1))) (Constr (Lambda ["_0"] (Exp (Constr (Case (Exp (Constr (Var "_0"))) [Constr (Alt (Pats [PLit (LAtom (Atom "true"))]) (Guard (Exp (Constr (Lit (LAtom (Atom "true")))))) (Exp (Constr (List (LL [Exp (Constr (Lit (LInt 116)))] (Exp (Constr (List (LL [Exp (Constr (Lit (LInt 114)))] (Exp (Constr (List (LL [Exp (Constr (Lit (LInt 117)))] (Exp (Constr (List (L [Exp (Constr (Lit (LInt 101)))]))))))))))))))))), Constr (Alt (Pats [PLit (LAtom (Atom "false"))]) (Guard (Exp (Constr (Lit (LAtom (Atom "true")))))) (Exp (Constr (List (LL [Exp (Constr (Lit (LInt 102)))] (Exp (Constr (List (LL [Exp (Constr (Lit (LInt 97)))] (Exp (Constr (List (LL [Exp (Constr (Lit (LInt 108)))] (Exp (Constr (List (LL [Exp (Constr (Lit (LInt 115)))] (Exp (Constr (List (L [Exp (Constr (Lit (LInt 101)))]))))))))))))))))))))), Constr (Alt (Pats [PVar "N"]) (Guard (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))), Exp (Constr (Lit (LAtom (Atom "is_list"))))) [Exp (Constr (Var "_0"))])))) (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))), Exp (Constr (Lit (LAtom (Atom "list_to_integer"))))) [Exp (Constr (Var "N"))])))), Constr (Alt (Pats [PVar "S"]) (Guard (Exp (Constr (Lit (LAtom (Atom "true")))))) (Exp (Constr (Var "S"))))]))))),
    FunDef (Constr (Function (Atom "length", 1))) (Constr (Lambda ["_0"] (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "list")))), Exp (Constr (Lit (LAtom (Atom "length"))))) [Exp (Constr (Var "_0"))]))))),
    FunDef (Constr (Function (Atom "print", 1))) (Constr (Lambda ["_0"] (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "io")))), Exp (Constr (Lit (LAtom (Atom "fwrite"))))) [Exp (Constr (List (LL [Exp (Constr (Lit (LInt 126)))] (Exp (Constr (List (LL [Exp (Constr (Lit (LInt 112)))] (Exp (Constr (List (LL [Exp (Constr (Lit (LInt 126)))] (Exp (Constr (List (L [Exp (Constr (Lit (LInt 110)))]))))))))))))))), Exp (Constr (List (LL [Exp (Constr (Var "_0"))] (Exp (Constr (Lit LNil))))))]))))),
    FunDef (Constr (Function (Atom "make", 0))) (Constr (Lambda [] (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))), Exp (Constr (Lit (LAtom (Atom "spawn"))))) [Exp (Constr (Lit (LAtom (Atom moduleName)))), Exp (Constr (Lit (LAtom (Atom "make")))), Exp (Constr (List (L [Exp (Constr (Lit LNil))])))]))))),
    FunDef (Constr (Function (Atom "make", 1))) (Constr (Lambda ["_0"] (Exp (Constr (Case (Exp (Constr (Var "_0"))) [Constr (Alt (Pats [PLit LNil]) (Guard (Exp (Constr (Lit (LAtom (Atom "true")))))) (Exp (Constr (Rec [Constr (Alt (Pats [PTuple [PLit (LAtom (Atom "recv")), PVar "PID"]]) (Guard (Exp (Constr (Lit (LAtom (Atom "true")))))) (Exp (Constr (App (Exp (Constr (Fun (Function (Atom "make", 1))))) [Exp (Constr (List (LL [Exp (Constr (Var "PID"))] (Exp (Constr (Lit LNil))))))]))))] (TimeOut (Exp (Constr (Lit (LAtom (Atom "infinity"))))) (Exp (Constr (Lit (LAtom (Atom "true")))))))))), Constr (Alt (Pats [PList (LL [PVar "X"] (PVar "Xs"))]) (Guard (Exp (Constr (Lit (LAtom (Atom "true")))))) (Exp (Constr (Rec [Constr (Alt (Pats [PTuple [PLit (LAtom (Atom "send")), PVar "Msg"]]) (Guard (Exp (Constr (Lit (LAtom (Atom "true")))))) (Exp (Constr (Let (["_1"], Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))), Exp (Constr (Lit (LAtom (Atom "self"))))) []))) (Exp (Constr (Seq (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))), Exp (Constr (Lit (LAtom (Atom "!"))))) [Exp (Constr (Var "X")), Exp (Constr (Tuple [Exp (Constr (Var "_1")), Exp (Constr (Var "Msg"))]))]))) (Exp (Constr (App (Exp (Constr (Fun (Function (Atom "make", 1))))) [Exp (Constr (Var "Xs"))])))))))))), Constr (Alt (Pats [PTuple [PLit (LAtom (Atom "recv")), PVar "PID"]]) (Guard (Exp (Constr (Lit (LAtom (Atom "true")))))) (Exp (Constr (Let (["_2"], Exp (Constr (List (LL [Exp (Constr (Var "X"))] (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))), Exp (Constr (Lit (LAtom (Atom "++"))))) [Exp (Constr (Var "Xs")), Exp (Constr (List (LL [Exp (Constr (Var "PID"))] (Exp (Constr (Lit LNil))))))]))))))) (Exp (Constr (App (Exp (Constr (Fun (Function (Atom "make", 1))))) [Exp (Constr (Var "_2"))])))))))] (TimeOut (Exp (Constr (Lit (LAtom (Atom "infinity"))))) (Exp (Constr (Lit (LAtom (Atom "true")))))))))), Ann (Alt (Pats [PVar "_3"]) (Guard (Exp (Constr (Lit (LAtom (Atom "true")))))) (Exp (Ann (Op (Atom "match_fail") [Exp (Constr (Tuple [Exp (Constr (Lit (LAtom (Atom "function_clause")))), Exp (Constr (Var "_3"))]))]) [CTuple [CLit (LAtom (Atom "function_name")), CTuple [CLit (LAtom (Atom "make")), CLit (LInt 1)]]]))) [CLit (LAtom (Atom "compiler_generated"))]]))))),
    FunDef (Constr (Function (Atom "send", 2))) (Constr (Lambda ["_0", "_1"] (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))), Exp (Constr (Lit (LAtom (Atom "!"))))) [Exp (Constr (Var "_0")), Exp (Constr (Tuple [Exp (Constr (Lit (LAtom (Atom "send")))), Exp (Constr (Var "_1"))]))]))))),
    FunDef (Constr (Function (Atom "recv", 1))) (Constr (Lambda ["_0"] (Exp (Constr (Let (["_1"], Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))), Exp (Constr (Lit (LAtom (Atom "self"))))) []))) (Exp (Constr (Seq (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))), Exp (Constr (Lit (LAtom (Atom "!"))))) [Exp (Constr (Var "_0")), Exp (Constr (Tuple [Exp (Constr (Lit (LAtom (Atom "recv")))), Exp (Constr (Var "_1"))]))]))) (Exp (Constr (Rec [Constr (Alt (Pats [PTuple [PVar "_3", PVar "Msg"]]) (Guard (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))), Exp (Constr (Lit (LAtom (Atom "=:="))))) [Exp (Constr (Var "_3")), Exp (Constr (Var "_0"))])))) (Exp (Constr (Var "Msg"))))] (TimeOut (Exp (Constr (Lit (LAtom (Atom "infinity"))))) (Exp (Constr (Lit (LAtom (Atom "true")))))))))))))))))
  ]