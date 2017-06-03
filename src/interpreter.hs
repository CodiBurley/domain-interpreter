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

-- Arithmetic Expressions
interpExpression (ArithmeticExp (Variable b)) e = interpExpression (ValueOf b) e

interpExpression (ArithmeticExp (Evaluate fc)) e = interpFuncCall fc e

interpExpression exp@(ArithmeticExp (IntLiteral i)) e = Just $ exp

interpExpression (ArithmeticExp (Negative a)) e = interpAsNegative a e

--interpExpression (ArithmeticExp aop@(ArithmeticOperation _ _ _)) e =
-- End Arithmetic Expressions



interpFuncCall :: FunctionCall -> Env -> Maybe Expression;
interpFuncCall (FunctionCall fe pe) e =
  case funcExp of
    Just (FunctionExp (Function s exp)) -> interpExpression exp e
    Just (FunctionExp (FunctionWithParam s pStr exp)) ->
      interpExpression exp ((pStr, paramExp):e)
    _                                   -> Nothing
  where funcExp = interpExpression fe e
        paramExp = interpExpression pe e

interpAsNegative :: Arithmetic -> Env -> Maybe Expression;
interpAsNegative arith e = 
  case (interpExpression (ArithmeticExp arith) e) of
    Just (ArithmeticExp (IntLiteral i)) ->
      interpExpression (ArithmeticExp (IntLiteral (-i))) e
    _                                   -> Nothing




envLookup :: Env -> String -> Maybe Expression
envLookup [] _ = Nothing
envLookup ((s, x):e) val = if s == val then x else (envLookup e val)




testFuncEval1 = interpExpression
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

basicFuncEvalWithParams = 
  (Evaluate
    (FunctionCall
      (FunctionExp
        (FunctionWithParam
          "doesntMatter"
          "x"
          (ArithmeticExp 
            (Variable (Value "x")))))
      (ArithmeticExp
        (IntLiteral 1))))

testFuncEvalParams1 =
  interpExpression (ArithmeticExp basicFuncEvalWithParams) []

testNegative1 =
  interpExpression (ArithmeticExp (Negative (IntLiteral 5))) []

testNegative2 =
  interpExpression (ArithmeticExp (Negative (Negative (IntLiteral 5)))) []


testNegative3 =
  interpExpression (ArithmeticExp (Negative basicFuncEvalWithParams)) []
