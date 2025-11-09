module FML.Parser.Component where

import Data.Char (isSpace, isUpper)
import Data.List (partition)
import FML.Grammar (Attribute (Attribute), FML (FMLComponent), FMLElement (FMLElement, FMLExpression, FMLLiteral, FMLCustomComponent), Prop (Prop))
import FML.Lib.Parser (Parser)
import FML.Parser.Utils (char, choice, identifier, lparen, operator, rparen, satisfyCond, string, whitespaces, zeroOrMore, (<?>), (<|>), peekChar)

-- A parser for one or more occurrences of `p`.
some' :: Parser a -> Parser [a]
some' p = do
  x <- p
  xs <- zeroOrMore p
  return (x : xs)

-- Helper to merge multiple class or id attributes into one.
mergeAttributes :: [Attribute] -> [Attribute]
mergeAttributes attrs =
  let (classAttrs, others1) = partition (\(Attribute name _) -> name == "class") attrs
      (idAttrs, finalOthers) = partition (\(Attribute name _) -> name == "id") others1

      -- Collect all values and join them with spaces.
      classValues = unwords [v | Attribute _ v <- classAttrs]
      idValues = unwords [v | Attribute _ v <- idAttrs]

      -- Create new merged attributes if any values were found.
      finalClassAttr = ([Attribute "class" classValues | not (null classValues)])
      finalIdAttr = ([Attribute "id" idValues | not (null idValues)])
   in finalClassAttr ++ finalIdAttr ++ finalOthers

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

-- A parser for a single prop
prop :: Parser Prop
prop = do
  name <- identifier
  whitespaces
  _ <- char ':'
  whitespaces
  -- The type can contain characters like `[]` for arrays, so we can't just use identifier
  -- Let's read until the next comma or closing parenthesis
  typeStr <- zeroOrMore (satisfyCond "a prop type" (`notElem` ",)"))
  return $ Prop name (trim typeStr)

-- A parser for a list of props in parentheses
props :: Parser [Prop]
props =
  lparen
    *> ( do
           p <- prop
           ps <- zeroOrMore (whitespaces *> char ',' *> whitespaces *> prop)
           return (p : ps)
           <|> return []
       )
    <* rparen

component :: Parser FML
component =
  ( do
      name <- identifier
      whitespaces
      -- Optional props
      props <- (props <|> return [])
      whitespaces
      operator "=>"
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
      let body = case children of
            [el] -> el
            _ -> FMLElement "fragment" [] children
      return $ FMLComponent name props body
  )
    <?> "a component definition (e.g., MyComponent => (div))"

customComponentElement :: Parser FMLElement
customComponentElement = do
  name <- identifier
  whitespaces
  children <-
    concat
      <$> zeroOrMore
        ( choice
            [ childrenInParens,
              inlineCompositionAsList
            ]
        )
  return $ FMLCustomComponent name children

childfreeElement :: Parser FMLElement
childfreeElement = do
  tag <- identifier
  raw_attributes <- zeroOrMore $ choice [propAttribute, idAttribute, classAttribute]
  let attributes = mergeAttributes raw_attributes
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
element =
  ( do
      tag <- identifier
      whitespaces
      raw_attributes <- zeroOrMore $ choice [propAttribute, idAttribute, classAttribute]
      let attributes = mergeAttributes raw_attributes
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
  )
    <?> "an element (e.g., div, p, h1)"

propAttribute :: Parser Attribute
propAttribute =
  ( do
      whitespaces
      propName <- identifier
      operator "="
      Attribute propName <$> string
  )
    <?> "a property attribute (e.g., name=\"value\")"

-- Parses a name part after a prefix (e.g., 'class' from '.class')
namePart :: Char -> Parser String
namePart prefix = do
  _ <- char prefix
  -- A name is a sequence of characters that are not delimiters.
  some' (satisfyCond "" (`notElem` " .#=()$,"))

classAttribute :: Parser Attribute
classAttribute =
  ( do
      whitespaces
      -- Parses one or more chained class names (e.g., .class1.class2)
      names <- some' (namePart '.')
      return $ Attribute "class" (unwords names)
  )
    <?> "a class attribute (e.g., .my-class)"

idAttribute :: Parser Attribute
idAttribute =
  ( do
      whitespaces
      -- Parses one or more chained id names (e.g., #id1#id2)
      names <- some' (namePart '#')
      return $ Attribute "id" (unwords names)
  )
    <?> "an id attribute (e.g., #my-id)"

inlineComposition :: Parser FMLElement
inlineComposition =
  ( do
      operator "$"
      childElement
  )
    <?> "an inline child element (e.g., $ p)"

expression :: Parser FMLElement
expression = do
  _ <- char '['
  whitespaces
  expr <- zeroOrMore (satisfyCond "a js expression" (/= ']'))
  whitespaces
  _ <- char ']'
  return $ FMLExpression expr

tryCustomComponentOrElement :: Parser FMLElement
tryCustomComponentOrElement = do
  mc <- peekChar
  case mc of
    Just c | isUpper c -> customComponentElement
    _ -> element

childElement :: Parser FMLElement
childElement = choice [literal, expression, tryCustomComponentOrElement] <?> "a child element (e.g. another element, or a string literal)"

literal :: Parser FMLElement
literal = (FMLLiteral <$> string) <?> "a string literal (e.g., \"some text\")"