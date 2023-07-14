module Lambda.Definition
  ( Definition (..),
    programParser,
    parseProgram,
  )
where

import Control.Applicative
import Data.Functor
import Lambda.Expression (Expression, Function, expressionParser, functionParser, nameSpan)
import Parser

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

programParser :: Parser (Expression, [Definition])
programParser = do
  x <- many (definitionParser <* ((charP '\n' *> wsP) <|> pure []))
  wsP
  final <- expressionParser
  return (final, x)

parseProgram :: String -> Maybe (Expression, [Definition])
parseProgram = finalize programParser
