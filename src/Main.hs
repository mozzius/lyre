module Main where

import Language.Compiler (compileModule)
import Language.CoreErlang.Parser (parseModule)
import Language.CoreErlang.Pretty (prettyPrint)
import Language.CoreErlang.Syntax (Module)
import Language.Parser (parse)
import Language.TypeChecker (typeCheck)
import System.Directory (createDirectoryIfMissing)
import System.Environment as Env (getArgs)
import System.Exit (exitSuccess)
import System.FilePath (takeBaseName, takeDirectory, (-<.>), (<.>), (</>))
import System.Process (callCommand)

-- https://stackoverflow.com/a/58685979
createAndWriteFile :: FilePath -> String -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path content

main :: IO ()
main = Env.getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ["-h"] = usage >> exitSuccess
parseArgs ["-v"] = version >> exitSuccess
parseArgs ["-p", path] = erl path >> exitSuccess
parseArgs ["-x", path] = run path >> exitSuccess
parseArgs [path] = compile path False 
parseArgs _ = usage >> exitSuccess 

compile :: String -> Bool -> IO ()
compile path printAST = do
  let name = takeBaseName path
  file <- readFile path
  let ast = (compileModule name . typeCheck $ parse file) :: Module
  if printAST then print ast else putStrLn "Compilation successful"
  createAndWriteFile ("build" </> name <.> "core") (prettyPrint ast)
  -- compile to BEAM
  callCommand ("erlc build" </> name <.> "core")
  putStrLn
    ( "Build successful\n"
        ++ "Output: "
        ++ name <.> "beam"
    )
  exitSuccess

usage :: IO ()
usage =
  putStrLn
    ( "Usage: lyre <path>     Compile Lyre code to BEAM\n"
        ++ "       lyre -x <path>  Execute BEAM code (WIP)\n"
        ++ "       lyre -v         Get Lyre version\n"
        ++ "       lyre -h         View available commands\n"
        ++ "       lyre -p <path>  Convert Erlang to CoreErlang"
    )

version :: IO ()
version = putStrLn "Lyre v0.0.1"

run :: String -> IO ()
run path = callCommand ("erl -noshell -s " ++ path ++ " main -s init stop")

erl :: FilePath -> IO ()
erl path = do
  let name = takeBaseName path
  callCommand ("erlc +to_core " ++ path)
  file <- readFile (name -<.> "core")
  print $ parseModule file
