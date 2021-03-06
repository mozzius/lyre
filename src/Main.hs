module Main where

import Language.CoreErlang.Syntax (Module)
import Language.CoreErlang.Pretty (prettyPrint)
import Language.CoreErlang.Parser (parseModule)
import System.Process (callCommand)
-- import System.Environment as Env
-- import System.Exit as Exit
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Language.Parser (parse)
import Language.TypeChecker (verify)
import Language.Compiler (compileModule)

-- https://stackoverflow.com/a/58685979
createAndWriteFile :: FilePath -> String -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path content

main :: IO ()
main = do
  file <- readFile "src\\test.lyre"
  let ast = (compileModule "test" . verify $ parse file) :: Module
  createAndWriteFile "build\\test.core" (prettyPrint ast)
  -- compile to BEAM
  callCommand "erlc build\\test.core"

  -- callCommand "erlc +to_core -o src src\\test.erl"
  -- file <- readFile "src\\test.core"

  -- print "===================="
  -- print file
  -- print "==== CoreErlang ===="
  -- print $ parseModule file
  -- print "===================="