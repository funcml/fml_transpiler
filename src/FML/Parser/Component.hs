module FML.Parser.Component where

import FML.Grammar (Attribute (Attribute), FML (FMLComponent), FMLElement (FMLElement, FMLLiteral))
import FML.Lib.Parser (Parser)
import FML.Parser.Utils (char, choice, identifier, lparen, operator, rparen, satisfyCond, string, whitespaces, zeroOrMore, (<|>))

component :: Parser FML
component = do
  name <- identifier
  operator "=>"
  lparen
  body <- choice [element, childfreeElement]
  rparen
  return $ FMLComponent name body

childfreeElement :: Parser FMLElement
childfreeElement = do
  tag <- identifier
  attributes <- zeroOrMore $ choice [propAttribute, idAttribute, classAttribute]
  whitespaces
  return $ FMLElement tag attributes []

childrenInParens :: Parser [FMLElement]
childrenInParens = do
  lparen
  whitespaces
  children <-
    ( do
        c <- childElement
        cs <- zeroOrMore (whitespaces *> char ',' *> whitespaces *> childElement)
        return (c : cs)
      )
      <|> return []
  whitespaces
  rparen
  return children

inlineCompositionAsList :: Parser [FMLElement]
inlineCompositionAsList = do
  el <- inlineComposition
  return [el]

childfreeElementAsList :: Parser [FMLElement]
childfreeElementAsList = do
  el <- childfreeElement
  return [el]

element :: Parser FMLElement
element = do
  tag <- identifier
  whitespaces
  attributes <- zeroOrMore $ choice [propAttribute, idAttribute, classAttribute]
  children <-
    concat
      <$> zeroOrMore
        ( choice
            [ childrenInParens,
              inlineCompositionAsList,
              childfreeElementAsList
            ]
        )
  return $ FMLElement tag attributes children

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
childElement = choice [element, literal, childfreeElement]

literal :: Parser FMLElement
literal = do
  FMLLiteral <$> string