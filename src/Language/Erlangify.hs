module Language.Erlangify where

-- module 'filename' ['hello'/0,
-- 	       'module_info'/0,
-- 	       'module_info'/1]
--     attributes [%% Line 1
-- 		'file' =
-- 		    %% Line 1
-- 		    [{ {- filename as list -} ,1}]]
-- -- ur code goes here
-- 'module_info'/0 =
--     fun () ->
-- 	call 'erlang':'get_module_info'
-- 	    ('filename')
-- 'module_info'/1 =
--     fun (_0) ->
-- 	call 'erlang':'get_module_info'
-- 	    ('filename', _0)
-- end

-- -- constructConstList :: [a] -> E.Exps
-- constructConstList []  = E.Exp (E.Constr (E.CLit E.LNil))
-- constructConstList [x] = (E.Exp (E.Constr (E.CList (E.L [x]))))
-- constructConstList (x:xs) =
--   (E.Exp (E.Constr (E.CList (E.LL [x] (constructConstList xs)))))

-- -- stringToConstList :: string -> E.Exps
-- stringToConstList str = constructConstList
--   $ map (\x -> E.Exp (E.Constr (E.CLit (E.LInt (toInteger $ ord x))))) str

-- constructLambda :: [string] -> E.Exps -> E.Constr


-- FunDef
--   (Constr (Function (Atom "module_info", 1)))
--   ( Constr
--     ( Lambda
--       ["_0"]
--       ( Exp
--         ( Constr
--           ( ModCall
--             ( Exp (Constr (Lit (LAtom (Atom "erlang"))))
--             , Exp (Constr (Lit (LAtom (Atom "get_module_info"))))
--             )
--             [ Exp (Constr (Lit (LAtom (Atom "test"))))
--             , Exp (Constr (Var "_0"))
--             ]
--           )
--         )
--       )
--     )
--   )