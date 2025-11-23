{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use guards" #-}
module FML.Parser.Component where

import Data.Char (isSpace, isUpper)
import Data.List (partition)
import FML.Grammar (Attribute (Attribute), AttributeValue (ExpressionValue, LiteralValue), FML (FMLComponent), FMLElement (FMLCustomComponent, FMLElement, FMLExpression, FMLListComprehension, FMLLiteral), Prop (Prop))
import FML.Lib.Parser (Parser)
import FML.Parser.Utils (char, choice, identifier, lparen, operator, peekChar, rparen, satisfyCond, string, whitespaces, zeroOrMore, (<?>), (<|>))

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
      classValues = unwords [v | Attribute _ (LiteralValue v) <- classAttrs]
      idValues = unwords [v | Attribute _ (LiteralValue v) <- idAttrs]

      -- Create new merged attributes if any values were found.
      finalClassAttr = ([Attribute "class" (LiteralValue classValues) | not (null classValues)])
      finalIdAttr = ([Attribute "id" (LiteralValue idValues) | not (null idValues)])
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
      componentProps <- props <|> return []
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
      return $ FMLComponent name componentProps body
  )
    <?> "a component definition (e.g., MyComponent => (div))"

customComponentElement :: Parser FMLElement
customComponentElement = do
  name <- identifier
  whitespaces
  raw_attributes <- zeroOrMore $ choice [propAttribute, idAttribute, classAttribute]
  let attributes = mergeAttributes raw_attributes
  children <-
    concat
      <$> zeroOrMore
        ( choice
            [ childrenInParens,
              inlineCompositionAsList
            ]
        )
  return $ FMLCustomComponent name attributes children

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

balancedBracketContent :: Int -> Parser String
balancedBracketContent 0 = return ""
balancedBracketContent level = do
  c <- satisfyCond "any character" (const True)
  case c of
    ']' -> (c :) <$> balancedBracketContent (level - 1)
    '[' -> (c :) <$> balancedBracketContent (level + 1)
    _ -> (c :) <$> balancedBracketContent level

expressionAttributeValue :: Parser AttributeValue
expressionAttributeValue = do
  _ <- char '['
  whitespaces
  expr <- balancedBracketContent 1
  whitespaces
  return $ ExpressionValue (init expr)

propAttribute ::
  Parser Attribute
propAttribute =
  ( do
      whitespaces
      propName <- identifier
      operator "="
      value <- (LiteralValue <$> string) <|> expressionAttributeValue
      return $ Attribute propName value
  )
    <?> "a property attribute (e.g., name=\"value\" or name=[value])"

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
      names <- some' (namePart '.')
      return $ Attribute "class" (LiteralValue (unwords names))
  )
    <?> "a class attribute (e.g., .my-class)"

idAttribute :: Parser Attribute
idAttribute =
  ( do
      whitespaces
      names <- some' (namePart '#')
      return $ Attribute "id" (LiteralValue (unwords names))
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
  expr <- balancedBracketContent 1
  whitespaces
  return $ FMLExpression (init expr)

tryCustomComponentOrElement :: Parser FMLElement
tryCustomComponentOrElement = do
  mc <- peekChar
  case mc of
    Just c | isUpper c -> customComponentElement
    _ -> element

-- Inline composition for list comprehension context
inlineCompositionInListComp :: Parser FMLElement
inlineCompositionInListComp = do
  operator "$"
  childElementInListComp

-- Element parser for list comprehension that handles inline composition correctly
elementInListComp :: Parser FMLElement
elementInListComp = do
  tag <- identifier
  whitespaces
  raw_attributes <- zeroOrMore $ choice [propAttribute, idAttribute, classAttribute]
  let attributes = mergeAttributes raw_attributes
  children <- zeroOrMore (whitespaces *> inlineCompositionInListComp)
  return $ FMLElement tag attributes children

-- Custom component parser for list comprehension
customComponentElementInListComp :: Parser FMLElement
customComponentElementInListComp = do
  name <- identifier
  whitespaces
  raw_attributes <- zeroOrMore $ choice [propAttribute, idAttribute, classAttribute]
  let attributes = mergeAttributes raw_attributes
  children <- zeroOrMore (whitespaces *> inlineCompositionInListComp)
  return $ FMLCustomComponent name attributes children

-- Try custom component or element in list comprehension context
tryCustomComponentOrElementInListComp :: Parser FMLElement
tryCustomComponentOrElementInListComp = do
  mc <- peekChar
  case mc of
    Just c | isUpper c -> customComponentElementInListComp
    _ -> elementInListComp

-- Helper to parse balanced content that stops at a comma at the current nesting level
balancedExprInListComp :: Parser String
balancedExprInListComp = fmap trim (go 0 0 0)
  where
    go :: Int -> Int -> Int -> Parser String
    go parens brackets braces = do
      mc <- peekChar
      case mc of
        Just ',' | parens == 0 && brackets == 0 && braces == 0 -> return ""
        Just ']' | parens == 0 && brackets == 0 && braces == 0 -> return ""
        Just c -> do
          _ <- satisfyCond "any char" (const True)
          let newParens = if c == '(' then parens + 1 else if c == ')' then parens - 1 else parens
          let newBrackets = if c == '[' then brackets + 1 else if c == ']' then brackets - 1 else brackets
          let newBraces = if c == '{' then braces + 1 else if c == '}' then braces - 1 else braces
          rest <- go newParens newBrackets newBraces
          return (c : rest)
        Nothing -> return ""

balancedExpr :: Parser String
balancedExpr = fmap trim (go 0 0 0)
  where
    go :: Int -> Int -> Int -> Parser String
    go parens brackets braces = do
      mc <- peekChar
      case mc of
        Just ',' | parens == 0 && brackets == 0 && braces == 0 -> return ""
        Just ']' | parens <= 0 && brackets <= 0 && braces <= 0 -> return ""
        Just c -> do
          _ <- satisfyCond "any char" (const True)
          let newParens = if c == '(' then parens + 1 else if c == ')' then parens - 1 else parens
          let newBrackets = if c == '[' then brackets + 1 else if c == ']' then brackets - 1 else brackets
          let newBraces = if c == '{' then braces + 1 else if c == '}' then braces - 1 else braces
          rest <- go newParens newBrackets newBraces
          return (c : rest)
        Nothing -> return ""

-- Better low-level expectation helpers
expectChar :: Char -> String -> Parser Char
expectChar c desc = char c <?> desc

expectOperator :: String -> String -> Parser ()
expectOperator op desc = operator op <?> desc

-- Explicit end-of-input without MonadFail; produce clearer message
endOfInput :: Parser ()
endOfInput = do
  mc <- peekChar
  case mc of
    Nothing -> return ()
    Just ch -> do
      -- Force a labeled failure; predicate always False
      _ <- satisfyCond ("no trailing input (unexpected '" ++ [ch] ++ "')") (const False)
      return ()

-- Public entry point with improved error reporting
topLevelComponent :: Parser FML
topLevelComponent =
  whitespaces *> (component <?> "a component definition") <* whitespaces <* endOfInput

destructuredObj :: Parser String
destructuredObj = do
  _ <- expectChar '{' "opening '{' for object destructuring"
  whitespaces
  content <- zeroOrMore (satisfyCond "object destructuring content" (/= '}'))
  whitespaces
  _ <- expectChar '}' "closing '}' for object destructuring"
  return $ "{" ++ trim content ++ "}"

destructuredArr :: Parser String
destructuredArr = do
  _ <- expectChar '[' "opening '[' for array destructuring"
  whitespaces
  content <- zeroOrMore (satisfyCond "array destructuring content" (/= ']'))
  whitespaces
  _ <- expectChar ']' "closing ']' for array destructuring"
  return $ "[" ++ trim content ++ "]"

destructuedTuple :: Parser String
destructuedTuple = do
  _ <- expectChar '(' "opening '(' for tuple destructuring"
  whitespaces
  content <- zeroOrMore (satisfyCond "tuple destructuring content" (/= ')'))
  whitespaces
  _ <- expectChar ')' "closing ')' for tuple destructuring"
  return $ "(" ++ trim content ++ ")"

-- Expression parser that stops at comma in list comprehension context
expressionInListComp :: Parser FMLElement
expressionInListComp = do
  _ <- char '['
  whitespaces
  expr <- balancedExprInListComp
  whitespaces
  _ <- char ']'
  return $ FMLExpression expr

-- Child element parser for list comprehension that handles comma boundaries
childElementInListComp :: Parser FMLElement
childElementInListComp = choice [literal, expressionInListComp, tryCustomComponentOrElementInListComp] <?> "a child element in list comprehension"

listComprehension :: Parser FMLElement
listComprehension =
  ( do
      _ <- expectChar '@' "start of list comprehension '@['"
      _ <- expectChar '[' "opening '[' after '@'"
      whitespaces
      elementToRender <- childElementInListComp <?> "element to render first (e.g., li)"
      whitespaces
      _ <- expectChar ',' "comma separating element and binding (e.g., li , x <- ...)"
      whitespaces
      var <-
        choice
          [ destructuredObj <?> "object destructuring (e.g., {a,b})",
            destructuredArr <?> "array destructuring (e.g., [a,b])",
            destructuedTuple <?> "tuple destructuring (e.g., (a,b))",
            identifier <?> "loop variable identifier"
          ]
      whitespaces
      _ <- expectOperator "<-" "binding operator '<-'"
      whitespaces
      listExpr <- balancedExpr <?> "list expression after '<-'"
      whitespaces
      filters <-
        zeroOrMore
          ( do
              _ <- expectChar ',' "comma before filter expression"
              whitespaces
              balancedExpr <?> "filter expression"
          )
      whitespaces
      _ <- expectChar ']' "closing ']' of list comprehension"
      return $ FMLListComprehension elementToRender var listExpr filters
  )
    <?> "a list comprehension (e.g., @[li, x <- xs, x > 0])"

childElement :: Parser FMLElement
childElement = choice [literal, expression, listComprehension, tryCustomComponentOrElement] <?> "a child element (e.g. another element, or a string literal)"

literal :: Parser FMLElement
literal = (FMLLiteral <$> string) <?> "a string literal (e.g., \"some text\")"