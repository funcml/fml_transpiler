module FML.Parser.Component where

import FML.Grammar (Attribute (Attribute), FML (FMLComponent), FMLElement (FMLElement, FMLLiteral))
import FML.Lib.Parser (Parser)
import FML.Parser.Utils (char, choice, identifier, lparen, operator, rparen, satisfyCond, string, whitespaces, zeroOrMore)

component :: Parser FML
component = do
  name <- identifier
  operator "=>"
  lparen
  body <- element
  rparen
  return $ FMLComponent name body

element :: Parser FMLElement
element = do
  tag <- identifier
  whitespaces
  attributes <- zeroOrMore $ choice [propAttribute, idAttribute, classAttribute]
  children <- zeroOrMore $ choice [inlineComposition, composition]
  return $ FMLElement tag attributes children

composition :: Parser FMLElement
composition = do
  lparen
  child <- childElement
  rparen
  return child

propAttribute :: Parser Attribute
propAttribute = do
  whitespaces
  propName <- identifier
  operator "="
  Attribute propName <$> string

classAttribute :: Parser Attribute
classAttribute = do
  whitespaces
  _ <- char '.'
  className <- zeroOrMore $ satisfyCond "" (\s -> s /= ' ' || s /= '.')
  return $ Attribute "class" className

idAttribute :: Parser Attribute
idAttribute = do
  whitespaces
  _ <- char '#'
  idName <- zeroOrMore $ satisfyCond "" (\s -> s /= ' ' || s /= '#')
  return $ Attribute "id" idName

inlineComposition :: Parser FMLElement
inlineComposition = do
  operator "$"
  childElement

childElement :: Parser FMLElement
childElement = choice [element, literal]

literal :: Parser FMLElement
literal = do
  FMLLiteral <$> string