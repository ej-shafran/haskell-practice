module Lambda.Parse (parseProgram) where

import Control.Applicative (many, (<|>))
import Data.Char (isSpace)
import Data.Functor (($>))
import Parser

-- Parser Utils

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

-- Function

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

-- Expression

data Expression
  = NameExpression String
  | FunctionExpression Function
  | ApplicationExpression {func :: Expression, arg :: Expression}
  deriving (Show)

applicationParser :: Parser Expression
applicationParser = do
  charP '('
  wsP
  func <- expressionParser
  charP ' '
  wsP
  arg <- expressionParser
  wsP
  charP ')'
  return (ApplicationExpression {func, arg})

expressionParser :: Parser Expression
expressionParser =
  (NameExpression <$> nameSpan)
    <|> applicationParser
    <|> (FunctionExpression <$> functionParser)

-- Definition

data Definition = Definition {name :: String, function :: Function} deriving (Show)

definitionParser :: Parser Definition
definitionParser = do
  literalP "def "
  wsP
  name <- nameSpan
  wsP
  charP '='
  wsP
  function <- functionParser
  return Definition {name, function}

-- Program

definitions :: Parser [Definition]
definitions = many $ definitionParser <* (charP '\n' *> wsP <|> pure [])

programParser :: Parser (Expression, [Definition])
programParser = do
  wsP
  defs <- definitions
  wsP
  final <- expressionParser
  wsP
  eofP
  return (final, defs)

parseProgram :: String -> Maybe (Expression, [Definition])
parseProgram = finalize programParser
