module Lambda.Expression
  ( Expression (..),
    nameSpan,
    nameParser,
    functionParser,
    expressionParser,
    parseExpression,
  )
where

import Control.Applicative
import Data.Char (isSpace)
import Data.Functor
import Parser

data Expression
  = Name String
  | Function {param :: String, body :: Expression}
  | Application {function :: Expression, argument :: Expression}
  deriving (Show)

isReservedChar :: Char -> Bool
isReservedChar c = isSpace c || c == '=' || c == '\\' || c == 'λ' || c == '•' || c == '(' || c == ')'

lambdaChar :: Parser ()
lambdaChar = do
  charP 'λ' <|> charP '\\'
  return ()

dotChar :: Parser ()
dotChar = do
  charP '•' <|> (charP '-' *> charP '>')
  return ()

nameSpan :: Parser String
nameSpan = nonEmptyP (spanP $ not . isReservedChar)

nameParser :: Parser Expression
nameParser = Name <$> nameSpan

applicationParser :: Parser Expression
applicationParser = do
  charP '('
  wsP
  function <- expressionParser
  charP ' '
  wsP
  argument <- expressionParser
  wsP
  charP ')'
  return (Application {function, argument})

functionParser :: Parser Expression
functionParser = do
  lambdaChar
  param <- nameSpan
  wsP
  dotChar
  wsP
  body <- expressionParser
  return (Function {param, body})

expressionParser :: Parser Expression
expressionParser = nameParser <|> applicationParser <|> functionParser

parseExpression :: String -> Maybe Expression
parseExpression = finalize (wsP *> expressionParser <* wsP <* eofP)
