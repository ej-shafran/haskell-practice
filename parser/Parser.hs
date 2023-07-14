{-# LANGUAGE LambdaCase #-}

module Parser
  ( Parser,
    runParser,
    finalize,
    eofP,
    charP,
    literalP,
    spanP,
    nonEmptyP,
    wsP,
    sepBy,
  )
where

import Control.Applicative
import Control.Monad
import Data.Char (isDigit, isSpace)
import Data.List.NonEmpty (nonEmpty)

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser fa) =
    Parser $
      \input -> do
        (input', a) <- fa input
        return (input', f a)

instance Applicative Parser where
  pure a = Parser (\input -> Just (input, a))
  (Parser pf) <*> (Parser pa) =
    Parser $
      \input -> do
        (input', f) <- pf input
        (input'', a) <- pa input'
        return (input'', f a)

instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
  (Parser pa) >>= f =
    Parser $
      \input -> do
        (input', a) <- pa input
        runParser (f a) input'

finalize :: Parser a -> String -> Maybe a
finalize (Parser p) input = snd <$> p input

eofP :: Parser ()
eofP = Parser $ \case
  [] -> Just ([], ())
  _ -> Nothing

charP :: Char -> Parser Char
charP c = Parser $ \case
  y : ys | y == c -> Just (ys, c)
  _ -> Nothing

literalP :: String -> Parser String
literalP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP pred = Parser $ \input ->
  let (init, rest) = span pred input
   in Just (rest, init)

nonEmptyP :: Parser [a] -> Parser [a]
nonEmptyP (Parser pa) =
  Parser $
    pa
      >=> (\(input', a) -> if null a then Nothing else Just (input', a))

wsP :: Parser String
wsP = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = ((:) <$> element <*> many (sep *> element)) <|> pure []
