{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module FML.Transpiler where

import FML.Grammar
  ( Attribute (..),
    AttributeValue (ExpressionValue, LiteralValue),
    FML (FMLComponent, FMLScript),
    FMLElement (..),
    FMLExpr (..),
    Prop (..),
    ScriptExpr (..),
  )

transpile :: [FML] -> String
transpile components = unlines $ map transpileComponent components

transpileComponent :: FML -> String
transpileComponent (FMLComponent name props body) =
  "export default function " ++ name ++ "(" ++ transpileProps props ++ ") {\n  return " ++ transpileElement body ++ ";\n}"
transpileComponent (FMLScript scriptExprs) =
  "// Script content\n" ++ unlines (map transpileScriptExpr scriptExprs)

transpileScriptExpr :: ScriptExpr -> String
transpileScriptExpr (FMLDeclaration (SignalDeclaration name typeStr value)) =
  "let [" ++ name ++ ", set" ++ name ++ "] = " ++ "createSignal(" ++ value ++ ");"
transpileScriptExpr (JSExpr s) = s
transpileScriptExpr _ = ""

transpileProps :: [Prop] -> String
transpileProps [] = ""
transpileProps props = "{ " ++ joinWithComma (map transpileProp props) ++ " }"

transpileProp :: Prop -> String
transpileProp (Prop name _) = name

transpileElement :: FMLElement -> String
transpileElement (FMLLiteral txt) =
  show txt
transpileElement (FMLExpression expr) =
  expr
transpileElement (FMLElement tag attrs children) =
  "f(" ++ show tag ++ "," ++ transpileAttributes attrs ++ transpileChildren children ++ ")"
transpileElement (FMLCustomComponent name attrs children) =
  name ++ "(" ++ transpileProps' attrs children ++ ")"
  where
    transpileProps' :: [Attribute] -> [FMLElement] -> String
    transpileProps' attributes childElements =
      let transpiledAttrs = map transpileAttr attributes
          transpiledChildren =
            if null childElements
              then []
              else ["children: " ++ childrenString]
          childrenString =
            if length childElements == 1
              then transpileElement (head childElements)
              else "[" ++ joinWithComma (map transpileElement childElements) ++ "]"
      in "{" ++ joinWithComma (transpiledAttrs ++ transpiledChildren) ++ "}"

transpileAttributes :: [Attribute] -> String
transpileAttributes [] = "{}"
transpileAttributes attrs =
  "{" ++ joinWithComma (map transpileAttr attrs) ++ "}"

transpileAttr :: Attribute -> String
transpileAttr (Attribute key (LiteralValue val))
  | key == "class" = showKeyVal "className" (show val)
  | otherwise = showKeyVal key (show val)
  where
    showKeyVal k v = show k ++ ": " ++ v
transpileAttr (Attribute key (ExpressionValue val))
  | key == "class" = showKeyVal "className" val
  | otherwise = showKeyVal key val
  where
    showKeyVal k v = show k ++ ": " ++ v

transpileChildren :: [FMLElement] -> String
transpileChildren [] = ""
transpileChildren xs =
  "," ++ joinWithComma (map transpileElement xs)

joinWithComma :: [String] -> String
joinWithComma [] = ""
joinWithComma [x] = x
joinWithComma (x : xs) = x ++ ", " ++ joinWithComma xs