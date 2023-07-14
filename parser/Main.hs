module Main where

import Lambda.Parse

main :: IO ()
main = do
  input <- getContents
  mapM_ print $ parseProgram input
