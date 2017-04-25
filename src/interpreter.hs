module Interpreter
(
) where
import Structure

type Env = [(String, Maybe Expression)]

interpExpressionEnv :: Expression -> Env -> (Maybe Expression, Env)
interpExpressionEnv exp@(Declare (BindValue s _)) e = (interped, (s, interped):e)
  where interped = interpExpression exp e
interpExpressionEnv exp e = (interpExpression exp e, e) 

interpExpression :: Expression -> Env -> Maybe Expression

interpExpression (ValueOf (Value s)) e = envLookup e s

interpExpression (Declare (BindValue s exp)) e = Just exp

-- TODO binding to functions?
--interpExpression (Declare (BindFunction (Function s exp))) e

interpExpression (ArithmeticExp (Variable b)) e = interpExpression (ValueOf b) e

interpExpression (ArithmeticExp (Evaluate fc)) e = interpFuncCall fc e


interpFuncCall :: FunctionCall -> Env -> Maybe Expression;
interpFuncCall (FunctionCall fe pe) e =
  case funcExp of
    Just (FunctionExp (Function s exp)) -> interpExpression exp e
    _                             -> Nothing
  where funcExp = interpExpression fe e

envLookup :: Env -> String -> Maybe Expression
envLookup [] _ = Nothing
envLookup ((s, x):e) val = if s == val then x else (envLookup e val)
