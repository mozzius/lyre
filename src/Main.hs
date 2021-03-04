module Main where

import Language.CoreErlang.Parser (parseModule)
import System.Process (callCommand)

import Language.Parser (parse)
import Language.TypeChecker (verify)
import Language.Compiler (stringToList)

main :: IO ()
main = generateCE

strToList = do
  print $ stringToList "hello"

parseTestFile = do
  file <- readFile "src\\test.lyre"
  print . verify $ parse file

generateCE = do
  callCommand "erlc +to_core -o src src\\test.erl "
  file <- readFile "src\\test.core"

  print "===================="
  print file
  print "==== CoreErlang ===="
  print $ parseModule file
  print "===================="