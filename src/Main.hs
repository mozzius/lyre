module Main where

import Language.CoreErlang.Syntax (Exps)
import Language.CoreErlang.Pretty (prettyPrint)
import Language.CoreErlang.Parser (parseModule)
import System.Process (callCommand)
-- import System.Environment as Env
-- import System.Exit as Exit

import Language.Parser (parse)
import Language.TypeChecker (verify)
import Language.Compiler (compile)

main :: IO ()
main = do
  -- file <- readFile "src\\test.lyre"
  -- let ast = compile . verify $ parse file
  -- print ast
  -- writeFile "build\\temp.core" (show ast)
  -- callCommand "erlc -o build build\\temp.core"

  callCommand "erlc +to_core -o src src\\test.erl"
  file <- readFile "src\\test.core"

  print "===================="
  print file
  print "==== CoreErlang ===="
  print $ parseModule file
  print "===================="