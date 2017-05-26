module Interpreter
(
) where
import Structure

type Env = [(String, Maybe Expression)]

-- Some interpretations have effects on the environment that
-- they were interpreted in. For example:
--   myvar is 6;
-- this expression evaluates to the integer constant 6, but it
-- also adds myvar to the environment is evaluated in.
-- 
-- interpExpressionEnv gives the interpretation of an expression, but
-- also the environment that is resulting from the inpretation. You 
-- can fold a script of expressions with this function to interpret the
-- script
interpExpressionEnv :: Expression -> Env -> (Maybe Expression, Env)
-- Interpretations that effect environment
interpExpressionEnv exp@(Declare (BindValue s _)) e = (interped, (s, interped):e)
  where interped = interpExpression exp e
interpExpressionEnv exp@(FunctionExp (Function s _)) e = (interped, (s, interped):e)
  where interped = interpExpression exp e

-- Implementations that have no effect on environment
interpExpressionEnv exp e = (interpExpression exp e, e) 


interpExpression :: Expression -> Env -> Maybe Expression

interpExpression (ValueOf (Value s)) e = envLookup e s

interpExpression (Declare (BindValue s exp)) e = Just exp

-- TODO binding to functions?
--interpExpression (Declare (BindFunction (Function s exp))) e

interpExpression exp@(FunctionExp fe) e = Just $ exp

interpExpression exp@(ArithmeticExp (IntLiteral i)) e = Just $ exp

interpExpression (ArithmeticExp (Variable b)) e = interpExpression (ValueOf b) e

interpExpression (ArithmeticExp (Evaluate fc)) e = interpFuncCall fc e


interpFuncCall :: FunctionCall -> Env -> Maybe Expression;
interpFuncCall (FunctionCall fe pe) e =
  case funcExp of
    Just (FunctionExp (Function s exp)) -> interpExpression exp e
    Just (FunctionExp (FunctionWithParam s pStr exp)) ->
      interpExpression exp ((pStr, paramExp):e)
    _                                   -> Nothing
  where funcExp = interpExpression fe e
        paramExp = interpExpression pe e

envLookup :: Env -> String -> Maybe Expression
envLookup [] _ = Nothing
envLookup ((s, x):e) val = if s == val then x else (envLookup e val)


test1 = interpExpression
  (ArithmeticExp
    (Evaluate
      (FunctionCall
        (FunctionExp
          (Function
            "doesntMatter"
            (ArithmeticExp 
              (IntLiteral 42))))
        (ArithmeticExp
          (IntLiteral 1)))))
  []

test2 = interpExpression
  (ArithmeticExp
    (Evaluate
      (FunctionCall
        (FunctionExp
          (FunctionWithParam
            "doesntMatter"
            "x"
            (ArithmeticExp 
              (Variable (Value "x")))))
        (ArithmeticExp
          (IntLiteral 1)))))
  []
