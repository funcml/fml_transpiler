{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Main where

import FML.AST (fml)
import FML.Grammar (printParseError)
import FML.Parser.Utils (run)
import FML.Transpiler (transpile)
import Foreign (Ptr)
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.Marshal.Alloc (mallocBytes)
import System.Environment (getArgs)

compileFML :: CString -> IO CString
compileFML cInput = do
  input <- peekCString cInput
  let output = compile input
  newCString output

foreign export ccall "compileFMLtoJS" compileFML :: CString -> IO CString

compile :: String -> String
compile input = case run fml input of
  Left err -> "Parse error: " ++ show err
  Right ast -> transpile ast

foreign export ccall "hsMalloc" hs_malloc :: Int -> IO (Ptr ())

hs_malloc :: Int -> IO (Ptr ())
hs_malloc = mallocBytes

main :: IO ()
main = return ()

-- main = do
--   args <- getArgs
--   case args of
--     [fileName] -> do
--       input <- readFile fileName
--       putStrLn input
--       -- putStrLn $ compile input
--       case run fml input of
--         Left err -> putStrLn $ "Parse error: " ++ show err
--         Right ast -> putStrLn $ "\nParsed AST:\n" ++ show ast
--     _ -> do
--       let testInput = "Counter => (div a=\"1\" b=\"2\" $ h1 $ \"lorem ipsum\" )"
--       putStrLn "Testing FML parser with input:\n"
--       putStrLn testInput
--       putStrLn ""

--       case run fml testInput of
--         Left err -> putStrLn $ printParseError err testInput
--         Right ast -> putStrLn $ "\nParsed AST:\n" ++ show ast

--       putStrLn "\nTranspiled output:\n"
--       case run fml testInput of
--         Left err -> putStrLn $ "Parse error: " ++ show err
--         Right ast -> putStrLn $ transpile ast