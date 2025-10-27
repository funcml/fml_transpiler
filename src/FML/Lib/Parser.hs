{-# LANGUAGE DeriveFunctor #-}

module FML.Lib.Parser where

import FML.Grammar (ParseError, Position)

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