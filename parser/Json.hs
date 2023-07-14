module Json
  ( JsonValue (..),
    parseJson,
  )
where

import Control.Applicative
import Data.Char (isDigit)
import Parser

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Float
  | JsonString String
  | JsonArray [JsonValue]
  | JsonRecord [(String, JsonValue)]
  deriving (Show)

commaSep :: Parser ()
commaSep = do
  wsP
  charP ','
  wsP
  return ()

digitSpan :: Parser [Char]
digitSpan = nonEmptyP $ spanP isDigit

postDecimalOptional :: Parser [Char]
postDecimalOptional = postDecimal <|> pure []
  where
    postDecimal = do
      charP '.'
      x <- digitSpan
      return ('.' : x)

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ literalP "null"

jsonBool :: Parser JsonValue
jsonBool = JsonBool . (== "true") <$> (literalP "true" <|> literalP "false")

jsonNumber :: Parser JsonValue
jsonNumber = do
  pre <- digitSpan
  post <- postDecimalOptional
  return (JsonNumber $ read $ pre ++ post)

stringLiteral :: Parser String
stringLiteral = do
  charP '"'
  x <- spanP (/= '"')
  charP '"'
  return x

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray = do
  charP '['
  wsP
  x <- sepBy (wsP *> charP ',' <* wsP) jsonValue
  wsP
  charP ']'
  return (JsonArray x)

keyVal :: Parser (String, JsonValue)
keyVal = do
  key <- stringLiteral
  wsP
  charP ':'
  wsP
  value <- jsonValue
  return (key, value)

jsonRecord :: Parser JsonValue
jsonRecord = do
  charP '{'
  wsP
  x <- sepBy (wsP *> charP ',' <* wsP) keyVal
  wsP
  charP '}'
  return (JsonRecord x)

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonRecord

parseJson :: String -> Maybe JsonValue
parseJson = finalize (wsP *> jsonValue <* wsP <* eofP)
