module Main where

import Exprs (erlangify)
import Parser (parseProgram)

main :: IO ()
main =
  do
    file <- readFile "test.lyre"
    print $ parseProgram file
    print . erlangify . parseProgram $ file
