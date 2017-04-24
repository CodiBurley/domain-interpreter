module Interpreter
(
) where
import Structure

type Env = [(String, Expression)]

interpretExpression :: Expression -> Env -> (Maybe Expression, Env)

interpretExpression (ValueOf (Value s)) e = (envLookup e s, e)

interpretExpression (Declare (BindValue s exp)) e = (Just exp, (s, exp):e)
  

envLookup :: Env -> String -> Maybe Expression
envLookup [] _ = Nothing
envLookup ((s, x):e) val = if s == val then Just x else (envLookup e val)
