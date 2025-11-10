{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use void" #-}
{-# HLINT ignore "Use replicateM" #-}
{-# HLINT ignore "Use isAsciiUpper" #-}
{-# HLINT ignore "Use isAsciiLower" #-}
{-# HLINT ignore "Use isDigit" #-}

module FML.Parser.Utils where

import Data.Char (isDigit)
import FML.Grammar
import FML.Lib.Parser

try :: Parser a -> Parser a
try p = Parser $ \(s, pos) ->
  case runParser p (s, pos) of
    (_, _, Left _) -> (s, pos, Left $ ParseError "failed, backtracking" (line pos) (column pos))
    success -> success

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser $ \(s, pos) ->
  case runParser p1 (s, pos) of
    res@(_, _, Right _) -> res
    (s', pos', Left err)
      | s' == s -> runParser p2 (s, pos)
      | otherwise -> (s', pos', Left err)

choice :: [Parser a] -> Parser a
choice [] = Parser $ \(s, pos) ->
  (s, pos, Left $ ParseError "no parsers provided to choice" (line pos) (column pos))
choice [p] = p
choice (p : ps) = p <|> choice ps

(<?>) :: Parser a -> String -> Parser a
p <?> msg = Parser $ \(s, pos) ->
  case runParser p (s, pos) of
    (s', _, Left _)
      | s' == s -> (s, pos, Left $ ParseError msg (line pos) (column pos))
    res -> res

infixl 1 <?>

run :: Parser a -> String -> Either ParseError a
run p s =
  case runParser (p <* eof) (s, Position 1 1) of
    (_, _, result) -> result

------------------------------------------------------------
-- POSITION TRACKING
------------------------------------------------------------

updatePosition :: Position -> Char -> Position
updatePosition (Position l _) '\n' = Position (l + 1) 1
updatePosition (Position l c) _ = Position l (c + 1)

------------------------------------------------------------
-- CHARACTER & PEEKING UTILITIES
------------------------------------------------------------

peekChar :: Parser (Maybe Char)
peekChar = Parser $ \(s, pos) -> case s of
  [] -> (s, pos, Right Nothing)
  (x : _) -> (s, pos, Right (Just x))

anyChar :: Parser Char
anyChar = Parser $ \case
  ([], pos) ->
    ([], pos, Left $ ParseError "unexpected end of text, expected any character" (line pos) (column pos))
  (x : xs, pos) ->
    (xs, updatePosition pos x, Right x)

eof :: Parser ()
eof = Parser $ \case
  ([], pos) -> ([], pos, Right ())
  (c : cs, pos) ->
    ( c : cs,
      pos,
      Left $
        ParseError
          ("expected end of input, but found '" ++ [c] ++ "'")
          (line pos)
          (column pos)
    )

zeroOrMore, oneOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []
oneOrMore p = liftA2 (:) p (zeroOrMore p)

sepByZeroOrMore, sepByOneOrMore :: Parser a -> Parser sep -> Parser [a]
sepByZeroOrMore p sep = sepByOneOrMore p sep <|> pure []
sepByOneOrMore p sep = liftA2 (:) p (zeroOrMore (sep *> p))

satisfyCond :: String -> (Char -> Bool) -> Parser Char
satisfyCond description predicate = Parser $ \(s, pos) ->
  case s of
    [] ->
      ( [],
        pos,
        Left $
          ParseError
            ("unexpected end of input, expected " ++ description)
            (line pos)
            (column pos)
      )
    (x : xs)
      | predicate x -> (xs, updatePosition pos x, Right x)
      | otherwise ->
          ( x : xs,
            pos,
            Left $
              ParseError
                ("expected " ++ description ++ ", found '" ++ [x] ++ "'")
                (line pos)
                (column pos)
          )

readUntilKeyword :: String -> Parser String
readUntilKeyword kw = Parser $ \(s, pos) -> go s pos ""
  where
    kwLen = length kw
    go [] p acc = ([], p, Right (reverse acc))
    go str@(x : xs) p acc
      | take kwLen str == kw = (str, p, Right (reverse acc))
      | otherwise = go xs (updatePosition p x) (x : acc)

keyword :: String -> Parser String
keyword kw = try $ Parser $ \(s, pos) ->
  if take (length kw) s == kw
    then
      let rest = drop (length kw) s
          newPos = foldl updatePosition pos (take (length kw) s)
       in (rest, newPos, Right kw)
    else (s, pos, Left $ ParseError ("expected keyword \"" ++ kw ++ "\"") (line pos) (column pos))

newline :: Parser Char
newline = satisfyCond "newline" (== '\n')

indent :: Parser String
indent = zeroOrMore (satisfyCond "space or tab" (\c -> c == ' ' || c == '\t'))

whitespaces :: Parser ()
whitespaces = zeroOrMore (satisfyCond "space, tab, or newline" (\c -> c == ' ' || c == '\t' || c == '\n')) >> pure ()

char :: Char -> Parser Char
char c = satisfyCond ("'" ++ [c] ++ "'") (== c)

isAlpha :: Char -> Bool
isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || (c >= '0' && c <= '9')

identifier :: Parser String
identifier = do
  firstChar <- satisfyCond "letter or '_' as the start of an identifier" (\c -> isAlpha c || c == '_')
  rest <- zeroOrMore (satisfyCond "alphanumeric or '_')" (\c -> isAlphaNum c || c == '_'))
  return (firstChar : rest)

number :: Parser String
number = oneOrMore (satisfyCond "a digit" isDigit)

string :: Parser String
string = do
  _ <- char '"'
  content <- zeroOrMore (satisfyCond "string character" (/= '"'))
  _ <- char '"'
  whitespaces
  return content

operator :: String -> Parser ()
operator c = do
  whitespaces
  _ <- keyword c
  whitespaces
  return ()

lparen, rparen :: Parser ()
lparen = operator "("
rparen = operator ")"