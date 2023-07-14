module Lambda.Expression
  ( Expression (..),
    Function (..),
    nameSpan,
    functionParser,
    expressionParser,
    parseExpression,
  )
where

import Control.Applicative
import Data.Char (isSpace)
import Data.Functor
import Parser

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

data Expression
  = NameExpression String
  | FunctionExpression Function
  | ApplicationExpression {function :: Expression, argument :: Expression}
  deriving (Show)

data Function = Function {param :: String, body :: Expression} deriving (Show)

functionParser :: Parser Function
functionParser = do
  lambdaChar
  param <- nameSpan
  wsP
  dotChar
  wsP
  body <- expressionParser
  return (Function {param, body})

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
  return (ApplicationExpression {function, argument})

expressionParser :: Parser Expression
expressionParser =
  (NameExpression <$> nameSpan)
    <|> applicationParser
    <|> (FunctionExpression <$> functionParser)

parseExpression :: String -> Maybe Expression
parseExpression = finalize (wsP *> expressionParser <* wsP <* eofP)
