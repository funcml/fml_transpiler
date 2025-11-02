{-# LANGUAGE LambdaCase #-}

module FML.AST where

import FML.Grammar (FML)
import FML.Lib.Parser (Parser)
import FML.Parser.Component (component)
import FML.Parser.Script (script)
import FML.Parser.Utils (choice, zeroOrMore, try)

fml :: Parser [FML]
fml = zeroOrMore (choice [try script, try component])