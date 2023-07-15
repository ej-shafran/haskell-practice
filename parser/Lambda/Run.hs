module Lambda.Run (runProgram) where

import Control.Applicative ((<|>))
import Data.List (find)
import Lambda.Parse
import Parser

lambdaApply :: [Definition] -> Expression -> Expression -> Maybe Function
lambdaApply defs f a = do
  f' <- getFunc defs f
  a' <- getFunc defs a
  callFunc defs f' a'

getFunc :: [Definition] -> Expression -> Maybe Function
getFunc defs (FunctionExpression f) = Just f
getFunc defs (NameExpression n) = function <$> find ((== n) . name) defs
getFunc defs (ApplicationExpression f a) = lambdaApply defs f a

replaceName :: Expression -> String -> Expression -> Expression
replaceName exp target goal = case exp of
  NameExpression n | n == target -> goal
  FunctionExpression (Function p b) -> FunctionExpression $ Function p (replaceName b target goal)
  ApplicationExpression f a -> ApplicationExpression (replaceName f target goal) (replaceName a target goal)
  n -> n

obfuscate :: Function -> Function
obfuscate (Function p b) = Function newName (replaceName b p (NameExpression newName))
  where
    newName = '_' : p

callFunc :: [Definition] -> Function -> Function -> Maybe Function
callFunc defs f a = getFunc defs $ replaceName (body f) (param f) (FunctionExpression a)

runExpression :: [Definition] -> Expression -> Maybe Function
runExpression defs (ApplicationExpression f a) = do
  result <- lambdaApply defs f a
  -- TODO: only do this if there are any name clashes
  return (obfuscate result)
runExpression defs exp = getFunc defs exp

runProgram input = do
  (exp, defs) <- parseProgram input
  runExpression defs exp
