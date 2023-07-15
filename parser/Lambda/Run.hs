module Lambda.Run (runProgram) where

import Control.Applicative ((<|>))
import Data.List (find)
import Lambda.Parse
import Parser

replaceName :: Expression -> [String] -> String -> Expression -> Expression
replaceName exp params target goal = case exp of
  NameExpression n | n == target && n `notElem` params -> goal
  FunctionExpression (Function p b) -> FunctionExpression $ Function p (replaceName b (p : params) target goal)
  ApplicationExpression f a -> ApplicationExpression (replaceName f params target goal) (replaceName a params target goal)
  n -> n

evaluate :: [Definition] -> Expression -> Maybe Function
evaluate defs (FunctionExpression f) = Just f
evaluate defs (NameExpression n) = do
  def <- find ((== n) . name) defs
  evaluate defs (expression def)
evaluate defs (ApplicationExpression f a) = do
  f' <- evaluate defs f
  call defs f' a

call :: [Definition] -> Function -> Expression -> Maybe Function
call defs (Function p b) exp = evaluate defs $ replaceName b [] p exp

runProgram :: String -> Maybe Function
runProgram input = do
  (exp, defs) <- parseProgram input
  evaluate defs exp
