module Structure
( Script(..),
  Expression(..),
  Domain(..),
  Arithmetic(..),
  Boolean(..),
  Function(..),
  FunctionCall(..),
  Conditional(..),
  Binding(..),
  Bound(..),
  OperatorArithmetic(..),
  OperatorBoolean(..),
  OperatorRelational(..)
) where


data Script = Script [Expression]
              deriving (Eq, Show)

data Expression = ValueOf Bound
                | Declare Binding
                | ArithmeticExp Arithmetic
                | BooleanExp Boolean
                | FunctionExp Function
                | FunctionEval FunctionCall
                | DomainExp Domain
                | ConditionExp Conditional
                  deriving (Eq, Show)

data Domain = Domain [String] [Binding] [Function]
              deriving (Eq, Show)

data Arithmetic = Variable Bound
                | Evaluate FunctionCall
                | IntLiteral Integer
                | Negative Arithmetic
                | ArithmeticOperation OperatorArithmetic Arithmetic Arithmetic
                  deriving (Eq, Show)

data Boolean = IsTrue
             | IsFalse
             | Negate Boolean
             | BooleanOperation OperatorBoolean Boolean Boolean
             | RelationalOperation OperatorRelational Arithmetic Arithmetic
               deriving (Eq, Show)

data Function = Function String Expression
              | FunctionWithParams String [String] Expression
                deriving (Eq, Show)

data FunctionCall = FunctionCall Expression Expression
                    deriving (Eq, Show)

data Conditional = IfThen Boolean Expression
                 | IfThenElse Boolean Expression Expression
                   deriving (Eq, Show)

data Binding = BindValue String Expression
             | BindFunction Function
               deriving (Eq, Show)

data Bound = Value String              -- value bound to name
           | DomainValue String String -- value bound to part of domain
             deriving (Eq, Show)

data OperatorArithmetic = Add | Subtract | Multiply | Divide
                          deriving (Eq, Show)

data OperatorBoolean = And | Or
                       deriving (Eq, Show)

data OperatorRelational = Greater | Less | Equal
                          deriving (Eq, Show)
