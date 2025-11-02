{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module FML.Transpiler where

import FML.Grammar
  ( Attribute (..),
    FML (FMLComponent),
    FMLElement (..),
  )

transpile :: [FML] -> String
transpile components = unlines $ map transpileComponent components

transpileComponent :: FML -> String
transpileComponent (FMLComponent name body) =
  "export default function " ++ name ++ "() {\n  return " ++ transpileElement body ++ ";\n}"

transpileElement :: FMLElement -> String
transpileElement (FMLLiteral txt) =
  show txt
transpileElement (FMLElement tag attrs children) =
  "f(" ++ show tag ++ "," ++ transpileAttributes attrs ++ transpileChildren children ++ ")"

transpileAttributes :: [Attribute] -> String
transpileAttributes [] = "{}"
transpileAttributes attrs =
  "{" ++ joinWithComma (map transpileAttr attrs) ++ "}"

transpileAttr :: Attribute -> String
transpileAttr (Attribute key val)
  | key == "class" = showKeyVal "className" val
  | otherwise = showKeyVal key val
  where
    showKeyVal k v = show k ++ ": " ++ show v

transpileChildren :: [FMLElement] -> String
transpileChildren [] = ""
transpileChildren xs =
  "," ++ joinWithComma (map transpileElement xs)

joinWithComma :: [String] -> String
joinWithComma = foldr1 (\a b -> a ++ ", " ++ b)
