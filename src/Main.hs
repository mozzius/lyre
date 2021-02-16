module Main where

-- import Erlangify (erlangify)

import Language.CoreErlang.Parser (parseModule)
import Language.Parser (parseProgram)
import System.Process

main :: IO ()
main =
  do
    -- file <- readFile "test.lyre"
    -- print $ parseProgram file
    -- print . erlangify . parseProgram $ file
    file <- readFile "./test.core"
    print $ parseModule file

    -- callCommand "erlc output.core"