module FML.Parser.Script where

import FML.Grammar (FML (FMLScript), FMLExpr (SignalDeclaration), ScriptExpr (FMLDeclaration, JSExpr))
import FML.Lib.Parser (Parser)
import FML.Parser.Utils (anyChar, char, identifier, lparen, number, operator, rparen, satisfyCond, string, whitespaces, zeroOrMore, (<|>))

defaultValueParser :: Parser String
defaultValueParser = zeroOrMore anyChar

-- signalDeclaration :: Parser FMLExpr
-- signalDeclaration = do
--   operator "~"
--   name <- identifier
--   whitespaces
--   _ <- char ':'
--   whitespaces
--   typeStr <- identifier
--   whitespaces
--   _ <- operator "="
--   whitespaces
--   defaultValue <- defaultValueParser
--   whitespaces -- Add this line
--   return $ SignalDeclaration name typeStr defaultValue

balancedParens :: Parser String
balancedParens = concat <$> zeroOrMore (nested <|> plain)
  where
    nested = do
      _ <- char '('
      inside <- balancedParens
      _ <- char ')'
      return ("(" ++ inside ++ ")")
    plain = fmap pure (satisfyCond "any character except '(' or ')'" (`notElem` "()"))

script :: Parser FML
script = do
  whitespaces
  _ <- operator "script"
  whitespaces
  _ <- char '('
  scriptContent <- balancedParens
  _ <- char ')'
  whitespaces
  return $ FMLScript [JSExpr scriptContent]
