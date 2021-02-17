module Main where

import Language.CoreErlang.Parser (parseModule)
import System.Process (callCommand)

import Language.Erlangify (stringToList)
import Language.Parser (parseProgram)

main :: IO ()
main = erlc

test = do
  print $ stringToList "hello"


parse = do
  file <- readFile "test.lyre"
  print $ parseProgram file
  print . parseProgram $ file

erlc = do
  callCommand "erlc +to_core -o src src\\test.erl "
  file <- readFile "src\\test.core"

  print "===================="
  print file
  print "==== CoreErlang ===="
  print $ parseModule file
  print "===================="