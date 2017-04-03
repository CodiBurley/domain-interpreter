module Parser
( parseString,
  parseFile
) where
import Lexer
import Structure
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Control.Monad

parseFile :: String -> IO Script
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

parseString :: String -> Script
parseString str =
  case parse whileParser "" str of
    Left e -> error $ show e
    Right r -> r

whileParser :: Parser Script
whileParser = whiteSpace >> script

script :: Parser Script
script = (expression `endBy` semi >>= return . Script)

expression :: Parser Expression
expression = try functionExp
         <|> try bindingExp
         <|> try domain
         <|> try functionCall
         <|> try booleanExp
         <|> try arithmeticExp
         <|> try boundExp
         <|> conditional

boundExp :: Parser Expression
boundExp = bound >>= return . ValueOf

bound :: Parser Bound
bound = try domainValue
    <|> (identifier >>= return . Value)

domainValue =
  do dom <- identifier
     reservedOp "."
     val <- identifier
     return $ DomainValue dom val

bindingExp :: Parser Expression
bindingExp = binding >>= return . Declare

binding :: Parser Binding
binding =
  do var <- identifier
     reservedOp "is"
     expr <- expression
     return $ BindValue var expr

arithmeticExp :: Parser Expression
arithmeticExp = arithmetic >>= return . ArithmeticExp

arithmetic :: Parser Arithmetic
arithmetic = buildExpressionParser aOperators aTerm

aOperators = [[Prefix (reservedOp "-" >> return (Negative))]
             ,[Infix  (reservedOp "*" >> return (ArithmeticOperation Multiply)) AssocLeft,
               Infix  (reservedOp "/" >> return (ArithmeticOperation Divide)) AssocLeft]
             ,[Infix  (reservedOp "+" >> return (ArithmeticOperation Add)) AssocLeft,
               Infix  (reservedOp "-" >> return (ArithmeticOperation Subtract)) AssocLeft]
             ]

aTerm = parens arithmetic
    <|> (bound >>= return . Variable)
    <|> liftM IntLiteral integer

booleanExp :: Parser Expression
booleanExp = boolean >>= return . BooleanExp

boolean :: Parser Boolean
boolean = buildExpressionParser bOperators bTerm

bOperators = [[Prefix (reservedOp "not" >> return (Negate))]
             ,[Infix  (reservedOp "and" >> return (BooleanOperation And)) AssocLeft,
               Infix  (reservedOp "or"  >> return (BooleanOperation Or)) AssocLeft]
             ]

bTerm = parens boolean
     <|> (reserved "true"  >> return IsTrue)
     <|> (reserved "false" >> return IsFalse)
     <|> relationalOperation

relationalOperation =
  do arith1 <- arithmetic
     op <- relationalOperator
     arith2 <- arithmetic
     return $ RelationalOperation op arith1 arith2

relationalOperator = (reservedOp ">"  >> return Greater)
                 <|> (reservedOp "<"  >> return Less)
                 <|> (reservedOp "==" >> return Equal)

functionExp :: Parser Expression
functionExp = function >>= return . FunctionExp

function :: Parser Function
function = try noParamFunction
       <|> paramFunction

noParamFunction =
  do var <- identifier
     reservedOp "="
     expr <- expression
     return $ Function var expr

paramFunction =
  do var <- identifier
     params <- parameters
     reservedOp "="
     expr <- expression
     return $ FunctionWithParams var params expr

parameters = sepBy1 identifier whiteSpace

functionCall :: Parser Expression
functionCall =
  do fExpr <-  (parens expression) <|> boundExp
     reservedOp "<-"
     expr <- expression
     return $ FunctionEval (FunctionCall fExpr expr)

domain :: Parser Expression
domain =
  do reserved "domain"
     params <- parameters
     reservedOp ":"
     reserved "values"
     reservedOp "{"
     values <- valuesList
     reservedOp "}"
     reserved "ranges"
     reservedOp "{"
     ranges <- rangesList
     reservedOp "}"
     return $ DomainExp (Domain params values ranges)

valuesList = sepBy1 binding (reservedOp ",")
rangesList = sepBy1 function (reservedOp ",")

conditional :: Parser Expression
conditional = (try ifThenElse >>= return . ConditionExp)
          <|> (ifThen >>= return . ConditionExp)

ifThen =
  do reserved "if"
     bool <- boolean
     reserved "then"
     iExpr <- expression
     return $ IfThen bool iExpr

ifThenElse =
  do reserved "if"
     bool <- boolean
     reserved "then"
     iExpr <- expression
     reserved "else"
     eExpr <- expression
     return $ IfThenElse bool iExpr eExpr
