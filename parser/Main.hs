module Main where

-- TODO: use Either instead of Maybe for better error reporting
import Lambda.Run

main :: IO ()
main = do
  input <- getContents
  let result = runProgram input
  case result of
    Just r -> print r
    Nothing -> putStrLn "Failed to parse..."
