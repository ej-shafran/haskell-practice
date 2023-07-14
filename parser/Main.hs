module Main where

import Lambda.Definition

main :: IO ()
main = do
  input <- getContents
  mapM_ print (parseProgram input)
