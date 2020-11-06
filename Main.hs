module Main where

import Parser (parseProgram)

main :: IO ()
main = do
  file <- readFile "test.lyre"
  print $ parseProgram file