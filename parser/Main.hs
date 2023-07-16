module Main where

-- TODO: use Either instead of Maybe for better error reporting

import Lambda.Parse (Function, parseDefinitions)
import Lambda.Run

runWithPrelude :: String -> String -> Maybe Function
runWithPrelude prelude program = do
  preludeDefs <- parseDefinitions prelude
  runProgram preludeDefs program

main :: IO ()
main = do
  input <- getContents
  prelude <- readFile "prelude.lambda"
  let result = runWithPrelude prelude input
  case result of
    Just r -> print r
    Nothing -> putStrLn "Failed to parse..."
