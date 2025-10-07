{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use void" #-}
{-# HLINT ignore "Use replicateM" #-}
{-# HLINT ignore "Use isAsciiUpper" #-}
{-# HLINT ignore "Use isAsciiLower" #-}
{-# HLINT ignore "Use isDigit" #-}

module Main where

data Attribute = Attribute String String deriving (Show, Eq)

data FMLElement = FMLText String Int | FMLTag String [Attribute] [FMLElement] Int deriving (Show, Eq)

data FML
  = FMLComponent String FMLElement
  deriving (Show, Eq)

data ParseError = ParseError String Int Int deriving (Show, Eq)

data Position = Position
  { line :: Int,
    column :: Int
  }
  deriving (Show, Eq)

newtype Parser a = Parser {runParser :: (String, Position) -> (String, Position, Either ParseError a)}
  deriving (Functor)

instance Applicative Parser where
  pure c = Parser $ \(s, pos) -> (s, pos, Right c)
  pf <*> pa = Parser $ \(s, pos) -> case runParser pf (s, pos) of
    (s', pos', Right f) -> case runParser pa (s', pos') of
      (s'', pos'', Right a) -> (s'', pos'', Right (f a))
      (s'', pos'', Left e) -> (s'', pos'', Left e)
    (s', pos', Left e) -> (s', pos', Left e)

instance Monad Parser where
  pa >>= f = Parser $ \(s, pos) -> case runParser pa (s, pos) of
    (s', pos', Right a) -> runParser (f a) (s', pos')
    (s', pos', Left e) -> (s', pos', Left e)

try :: Parser a -> Parser a
try p = Parser $ \(s, pos) -> case runParser p (s, pos) of
  (_s', _pos', Left err) -> (s, pos, Left err)
  success -> success

-- An alternative/choice operator that provides better error messages.
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser $ \(s, pos) -> case runParser p1 (s, pos) of
  res@(_, _, Right _) -> res
  (s', pos', Left err)
    -- If p1 failed without consuming input, try p2.
    | s' == s -> runParser p2 (s, pos)
    -- Otherwise, p1 failed after consuming input, so we report its error.
    | otherwise -> (s', pos', Left err)

-- A choice between multiple parsers.
choice :: [Parser a] -> Parser a
choice [] = Parser $ \(s, pos) -> (s, pos, Left $ ParseError "no parsers provided to choice" (line pos) (column pos))
choice [p] = p
choice (p : ps) = p <|> choice ps

-- Run a parser on a string.
run :: Parser a -> String -> Either ParseError a
run p s = case runParser (p <* eof) (s, Position 1 1) of
  (_, _, result) -> result

-- Update position when consuming characters
updatePosition :: Position -> Char -> Position
updatePosition (Position l _) '\n' = Position (l + 1) 1
updatePosition (Position l c) _ = Position l (c + 1)

zeroOrMore, oneOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []
oneOrMore p = liftA2 (:) p (zeroOrMore p)

sepByZeroOrMore, sepByOneOrMore :: Parser a -> Parser sep -> Parser [a]
sepByZeroOrMore p sep = sepByOneOrMore p sep <|> pure []
sepByOneOrMore p sep = liftA2 (:) p (zeroOrMore (sep *> p))

anyChar :: Parser Char
anyChar = Parser $ \case
  ([], pos) -> ("", pos, Left $ ParseError "unexpected end of text, expected any character, but found end of input" (line pos) (column pos))
  (x : xs, pos) -> (xs, updatePosition pos x, Right x)

eof :: Parser ()
eof = Parser $ \case
  ([], pos) -> ("", pos, Right ())
  (s@(c : _), pos) -> (s, pos, Left $ ParseError ("expected end of input, but found '" ++ [c] ++ "'") (line pos) (column pos))

satisfyCond :: String -> (Char -> Bool) -> Parser Char
satisfyCond description predicate = try $ do
  c <- anyChar
  if predicate c
    then pure c
    else Parser $ \(s, pos) -> (s, pos, Left $ ParseError ("expected " ++ description ++ ", but found '" ++ [c] ++ "'") (line pos) (column pos - 1))

keyword :: String -> Parser String
keyword kw = try $ do
  let kwLen = length kw
  parsed <- sequenceA (replicate kwLen anyChar)
  if parsed == kw
    then pure kw
    else Parser $ \(s, pos) -> (s, pos, Left $ ParseError ("expected keyword \"" ++ kw ++ "\", but found \"" ++ parsed ++ "\"") (line pos) (column pos - kwLen))

newline :: Parser Char
newline = satisfyCond "newline" (== '\n')

-- This parser is intentionally not exported to avoid confusion with skipSpaces
indent :: Parser String
indent = zeroOrMore (satisfyCond "space or tab" (\c -> c == ' ' || c == '\t'))

skipSpaces :: Parser ()
skipSpaces = zeroOrMore (satisfyCond "space, tab, or newline" (\c -> c == ' ' || c == '\t' || c == '\n')) >> pure ()

char :: Char -> Parser Char
char c = satisfyCond ("'" ++ [c] ++ "'") (== c)

isAlpha :: Char -> Bool
isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || (c >= '0' && c <= '9')

parseIdentifier :: Parser String
parseIdentifier = do
  firstChar <- satisfyCond "letter or '_" (\c -> isAlpha c || c == '_')
  rest <- zeroOrMore (satisfyCond "letter, digit, or '_" (\c -> isAlphaNum c || c == '_'))
  return (firstChar : rest)

parseStringLiteral :: Parser String
parseStringLiteral = do
  _ <- char '"'
  content <- zeroOrMore (satisfyCond "any character except '\"'" (/= '"'))
  _ <- char '"'
  return content

parseAttribute :: Parser Attribute
parseAttribute = do
  skipSpaces
  name <- parseIdentifier
  skipSpaces
  _ <- char '='
  skipSpaces
  Attribute name <$> parseStringLiteral

-- The main FML parser. It must be tried at the start of any content.
fml :: Parser FML
fml = skipSpaces >> choice [parseComponent]

parseComponent :: Parser FML
parseComponent = do
  skipSpaces
  _ <- char ':'
  _ <- keyword "component"
  skipSpaces
  name <- parseIdentifier
  skipSpaces
  _ <- char '('
  skipSpaces
  element <- parseRootElement
  skipSpaces
  _ <- char ')'
  skipSpaces

  return $
    FMLComponent
      name
      element

parseRootElement :: Parser FMLElement
parseRootElement = do
  skipSpaces
  currentLevel <- getCurrentIndentation
  tagName <- parseIdentifier
  -- attrs <- zeroOrMore parseAttribute

  skipSpaces
  children <- parseChildren currentLevel
  return $ FMLTag tagName [] children currentLevel

getCurrentIndentation :: Parser Int
getCurrentIndentation = Parser $ \(s, pos) -> (s, pos, Right (column pos - 1))

parseChildren :: Int -> Parser [FMLElement]
parseChildren parentLevel = zeroOrMore $ try $ do
  skipSpaces
  _ <- keyword "->"
  skipSpaces
  currentLevel <- getCurrentIndentation
  if currentLevel > parentLevel
    then parseRootElement
    else
      if currentLevel == parentLevel
        then do
          _ <- keyword ":>"
          skipSpaces
          text <- parseStringLiteral
          return $ FMLText text currentLevel
        else Parser $ \(s, pos) -> (s, pos, Left $ ParseError ("Wrong indentation: expected greater than " ++ show parentLevel ++ " spaces, got " ++ show currentLevel) (line pos) (column pos))

-- parseElement :: Parser FMLElement
-- parseElement = choice [parseFMLTag, parseText]

-- parseFMLChildren :: Parser [FMLElement]

-- testFML :: String -> IO ()
-- testFML input = case run fml input of
--   Left err -> putStrLn $ "Parse error: " ++ show err
--   Right ast -> putStrLn $ printFML ast

main :: IO ()
-- :component BasicFML
--     (
--         div
--         -> h1
--             :> "Hello, world!"
--     )
main = do
  let testInput =
        -- unlines
        --   [ ":component BasicFML",
        --     "  (",
        --     "    div",
        --     "      :> \"Hello, world!\"",
        --     "  )"
        --   ]
        unlines
          [ ":component BasicFML",
            "    (",
            "        div",
            "        -> h1",
            "          -> h5",
            "            -> h6",
            "            :> \"Hello, world!\"",
            "        -> h2",
            "          -> h3",
            "          -> h4",
            "    )"
          ]
  putStrLn "Testing FML parser with input:\n"
  putStrLn testInput

  case run fml testInput of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right ast -> putStrLn $ "\nParsed AST:\n" ++ show ast

-- putStrLn "\nParsed AST:\n"
-- testFML testInput