module Main where

import Exprs ()
import Language.CoreErlang ()
import Parser (parseProgram)

main :: IO ()
main = do
  file <- readFile "test.lyre"
  print $ parseProgram file
--   print $ parseStatements $ parseProgram file

-- parseStatements x = map parseExpressions x

-- parseExpressions x = case x of
--   FuncDef name args (Curly block) -> ("Function" ++ name ++ show args, parseStatements block) 
--   _ -> (show x,[])
