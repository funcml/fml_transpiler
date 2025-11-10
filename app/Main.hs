{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Main where

import FML.AST (fml)
import FML.Grammar (printParseError)
import FML.Parser.Utils (run)
import FML.Transpiler (transpile)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

compile :: String -> String
compile input = case run fml input of
  Left err -> "Parse error: " ++ show err
  Right ast -> transpile ast

runTest :: (String, String) -> IO ()
runTest (name, input) = do
  putStrLn $ "--- Testing: " ++ name ++ " ---"
  putStrLn $ "Input: " ++ input
  case run fml input of
    Left err -> putStrLn $ "Parse error: " ++ printParseError err input
    Right ast -> do
      putStrLn $ "AST: " ++ show ast
      putStrLn $ "Output: " ++ transpile ast
  putStrLn ""

testTranspile :: IO ()
testTranspile = do
  putStrLn "--- Running tests ---"
  let tests =
        [ ("Script with signal declaration", "script ( ~ count: number = new Date() )"),
          ("Script with component", "script (const x = new Date() )\n MyComponent => (div([1 + 1]))"),
          ("Component with expression", "MyComponent => (div([1 + 1]))"),
          ("Component with props", "MyComponent (name: string, age: number) => (div)"),
          ("Component with a child-free element", "MyComponent => (p)"),
          ("Component with childfree children", "MyComponent => (p, span)"),
          ("Component with list of children", "MyComponent => (ul (li, li, li))"),
          ("Component with inline child", "MyComponent => (p $ span $ \"hello\")"),
          ("Mixed children", "MyComponent => (div (p, span) $ h1 $ \"title\" img)"),
          ("Empty parens for children", "MyComponent => (div ())"),
          ("Nested", "MyComponent => (div (p (span \"text\")))"),
          ("Single child in parens", "MyComponent => (p (span))"),
          ("Tricky case with child and sibling", "MyComponent => (div (p) span)"),
          ("Composition", "A => (h1 $ \"A\")\nB => (h1 x=\"1\" $ A y=\"2\")"),
          ("Custom component with children", "MyComponent => (CustomComp (div, span, AnotherComp))"),
          ("Js Expr in Attr", "MyComponent => (div props=[(e) => setTodos([...todos, e.target.value])])")
        ]
  mapM_ runTest tests
  putStrLn "--- Tests finished ---"

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
    ("--test" : _) -> do
      testTranspile
      exitSuccess
    _ -> do
      hPutStrLn stderr "Unknown command: use --help for more info"
      exitFailure
