{-# LANGUAGE LambdaCase #-}

module FML.AST where

import FML.Grammar (FML)
import FML.Lib.Parser (Parser)
import FML.Parser.Component (component)
import FML.Parser.Utils (choice, zeroOrMore)

components :: Parser [FML]
components = zeroOrMore component

fml :: Parser [FML]
fml = choice [components]