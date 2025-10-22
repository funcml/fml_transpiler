{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use void" #-}
{-# HLINT ignore "Use replicateM" #-}
{-# HLINT ignore "Use isAsciiUpper" #-}
{-# HLINT ignore "Use isAsciiLower" #-}
{-# HLINT ignore "Use isDigit" #-}
module FML.Parser.Utils where

import FML.Grammar
import FML.Lib.Parser

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
    else Parser $ \(s, pos) -> (s, pos, Left $ ParseError ("expected " ++ description) (line pos) (column pos - 1))

readUntilKeyword :: String -> Parser String
readUntilKeyword kw = Parser $ \(s, pos) -> go s pos ""
  where
    kwLen = length kw
    go [] p acc = ([], p, Right (reverse acc))
    go str@(x : xs) p acc
      | take kwLen str == kw = (str, p, Right (reverse acc))
      | otherwise = go xs (updatePosition p x) (x : acc)

keyword :: String -> Parser String
keyword kw = try $ do
  let kwLen = length kw
  parsed <- sequenceA (replicate kwLen anyChar)
  if parsed == kw
    then pure kw
    else Parser $ \(s, pos) -> (s, pos, Left $ ParseError ("expected " ++ kw) (line pos) (column pos - kwLen))

newline :: Parser Char
newline = satisfyCond "newline" (== '\n')

-- This parser is intentionally not exported to avoid confusion with whitespaces
indent :: Parser String
indent = zeroOrMore (satisfyCond "space or tab" (\c -> c == ' ' || c == '\t'))

whitespaces :: Parser ()
whitespaces = zeroOrMore (satisfyCond "space, tab, or newline" (\c -> c == ' ' || c == '\t' || c == '\n')) >> pure ()

char :: Char -> Parser Char
char c = satisfyCond ("'" ++ [c] ++ "'") (== c)

isAlpha :: Char -> Bool
isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

operator :: String -> Parser ()
operator c = do
  whitespaces
  _ <- keyword c
  whitespaces
  return ()

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || (c >= '0' && c <= '9')

identifier :: Parser String
identifier = do
  firstChar <- satisfyCond "letter or '_' as the start of an identifier" (\c -> isAlpha c || c == '_')
  rest <- zeroOrMore (satisfyCond "invalid keyword or identifier" (\c -> isAlphaNum c || c == '_'))
  return (firstChar : rest)

string :: Parser String
string = do
  _ <- char '"'
  content <- zeroOrMore (satisfyCond "a closing \" at the end of a string literal" (/= '"'))
  _ <- char '"'
  whitespaces
  return content

lparen :: Parser ()
lparen = do
  _ <- operator "("
  return ()

rparen :: Parser ()
rparen = do
  _ <- operator ")"
  return ()