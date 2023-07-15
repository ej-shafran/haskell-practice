module Lambda.Run (runProgram) where

import Control.Applicative ((<|>))
import Data.List (find)
import Lambda.Parse
import Parser

replaceName :: Expression -> [String] -> String -> Expression -> Expression
replaceName exp safe target goal = case exp of
  NameExpression n | n == target && n `notElem` safe -> goal
  FunctionExpression (Function p b) -> FunctionExpression $ Function p (replaceName b (p : safe) target goal)
  ApplicationExpression f a -> ApplicationExpression (replaceName f safe target goal) (replaceName a safe target goal)
  n -> n

evaluate :: [Definition] -> Expression -> Maybe Function
-- TODO: maybe replace definitions inside the function body?
evaluate defs (FunctionExpression f) = Just f
evaluate defs (NameExpression n) = do
  def <- find ((== n) . name) defs
  evaluate defs (expression def)
evaluate defs (ApplicationExpression f a) = do
  f' <- evaluate defs f
  a' <- evaluate defs a
  call defs f' a'

call :: [Definition] -> Function -> Function -> Maybe Function
call defs (Function p b) a = evaluate defs $ replaceName b [] p (FunctionExpression a)

runProgram :: String -> Maybe Function
runProgram input = do
  (exp, defs) <- parseProgram input
  evaluate defs exp
