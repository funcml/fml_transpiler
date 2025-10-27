{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Main where

import FML.AST (fml)
import FML.Parser.Utils (run)
import FML.Transpiler (transpile)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

compile :: String -> String
compile input = case run fml input of
  Left err -> "Parse error: " ++ show err
  Right ast -> transpile ast

printHelp :: IO ()
printHelp = do
  putStrLn "fmlc — FML Compiler"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  fmlc -c <source>     Compile inline FML source to JavaScript"
  putStrLn "  fmlc --help          Show this help message"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  fmlc -c \"div(class='greet') 'Hello world'\""
  putStrLn ""
  putStrLn "Notes:"
  putStrLn "  The compiler reads your FML markup and outputs JavaScript that directly injects components into the DOM."

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("-c" : src : _) -> do
      let output = compile src
      putStrLn output
    ("--help" : _) -> do
      printHelp
      exitSuccess
    _ -> do
      hPutStrLn stderr "Unknown command: use --help for more info"
      exitFailure
