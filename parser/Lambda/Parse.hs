module Lambda.Parse
  ( parseProgram,
    Expression (..),
    Definition (..),
    Function (..),
    parseDefinitions,
  )
where

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

data Function = Function {param :: String, body :: Expression}

instance Show Function where
  show :: Function -> String
  show (Function p b) = 'λ' : p ++ " • " ++ show b

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

instance Show Expression where
  show :: Expression -> String
  show (NameExpression n) = n
  show (FunctionExpression f) = show f
  show (ApplicationExpression f a) = '(' : show f ++ " " ++ show a ++ ")"

applicationParser :: Parser Expression
applicationParser = do
  charP '('
  wsP
  func <- partExpressionParser
  wsP
  arg <- partExpressionParser
  wsP
  charP ')'
  return (ApplicationExpression {func, arg})

functionExpressionParser :: Parser Expression
functionExpressionParser = FunctionExpression <$> functionParser

nameExpressionParser :: Parser Expression
nameExpressionParser = NameExpression <$> nameSpan

partExpressionParser :: Parser Expression
partExpressionParser = functionExpressionParser <|> applicationParser <|> nameExpressionParser

expressionParser :: Parser Expression
expressionParser =
  functionExpressionParser
    <|> applicationParser
    <|> complexAppParser
    <|> nameExpressionParser

-- TODO
complexAppParser :: Parser Expression
complexAppParser = do
  func <- partExpressionParser
  charP ' '
  arg <- partExpressionParser
  exps <- many $ charP ' ' *> expressionParser
  return $ foldl ApplicationExpression (ApplicationExpression {func, arg}) exps

-- Definition

data Definition = Definition {name :: String, expression :: Expression} deriving (Show)

simpleDefParser :: Parser Definition
simpleDefParser = do
  literalP "def "
  wsP
  name <- nameSpan
  wsP
  charP '='
  wsP
  expression <- expressionParser
  return Definition {name, expression}

complexDefParser :: Parser Definition
complexDefParser = do
  literalP "def "
  wsP
  name <- nameSpan
  wsP
  params <- many $ nameSpan <* wsP
  charP '='
  wsP
  expression <- expressionParser
  let final = foldr (\prm exp -> FunctionExpression $ Function prm exp) expression params
  return Definition {name = name, expression = final}

definitionParser :: Parser Definition
definitionParser = simpleDefParser <|> complexDefParser

-- Comments

commentParser :: Parser ()
commentParser = do
  charP '#'
  spanP (/= '\n')
  charP '\n'
  wsP
  return ()

-- Program

defOrComment :: Parser Definition
defOrComment = do
  many commentParser
  -- TODO: maybe remove this?
  wsP
  definitionParser <* (charP '\n' *> wsP <|> pure [])

definitions :: Parser [Definition]
definitions = many defOrComment

parseDefinitions :: String -> Maybe [Definition]
parseDefinitions = finalize definitions

programParser :: Parser (Expression, [Definition])
programParser = do
  wsP
  defs <- definitions
  wsP
  many commentParser
  final <- expressionParser
  wsP
  many commentParser
  eofP
  return (final, defs)

parseProgram :: String -> Maybe (Expression, [Definition])
parseProgram = finalize programParser
