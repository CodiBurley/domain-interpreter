module ParserTest
(runTests
) where
import Parser
import Structure

assertFileParsesTo :: String -> Script -> IO Bool
assertFileParsesTo fileName expectedResult =
  do parseResult <- parseFile fileName
     return $ parseResult == expectedResult

assertSimpleArith =
  assertFileParsesTo
    "../test/parse/simple-arith.dom"
    (Script [
      ArithmeticExp (
        ArithmeticOperation
          Subtract
          (ArithmeticOperation Add (IntLiteral 5) (ArithmeticOperation Multiply (IntLiteral 4) (IntLiteral 9)))
          (IntLiteral 3))])

assertSimpleBool =
  assertFileParsesTo
    "../test/parse/simple-bool.dom"
    (Script [
      BooleanExp (
        BooleanOperation
          And
          (BooleanOperation
            And
            IsTrue
            (BooleanOperation
              Or
              (RelationalOperation
                Greater
                  (IntLiteral 4)
                  (IntLiteral 2))
              IsFalse))
          (Negate
            (RelationalOperation
              Equal
              (IntLiteral 3)
              (IntLiteral 3))))])

assertSimpleBinding =
  assertFileParsesTo
  "../test/parse/simple-binding.dom"
  (Script [
    Declare (
      BindValue
        "myval"
        (ArithmeticExp
          (IntLiteral 8))),
    Declare(
      BindValue
      "myval"
      (ArithmeticExp
        (Variable
          (Value "varname"))))])

assertSimpleFunction =
  assertFileParsesTo
  "../test/parse/simple-function.dom"
  (Script [
    FunctionExp (
      Function
        "twoTimesThree"
        (ArithmeticExp (
          ArithmeticOperation
            Multiply
            (IntLiteral 2)
            (IntLiteral 3)))),
    FunctionExp (
      FunctionWithParam
        "timesTwo"
        "num"
        (ArithmeticExp (
          ArithmeticOperation
            Multiply
            (Variable (Value "num"))
            (IntLiteral 2))))])

assertSimpleApplication =
  assertFileParsesTo
  "../test/parse/simple-application.dom"
  (Script [
    FunctionEval (
      FunctionCall
        (ValueOf (Value "timeTwo"))
        (ArithmeticExp (IntLiteral 4))),
    FunctionEval (
      FunctionCall
        (ValueOf (Value "timesTwo"))
        (ArithmeticExp (
          ArithmeticOperation
            Multiply
            (IntLiteral 2)
            (Variable (Value "myvar")))))])

assertSimpleDomain =
  assertFileParsesTo
  "../test/parse/simple-domain.dom"
  (Script [
    Declare (
      BindValue
        "Complex"
        (DomainExp (
          Domain
            ["i_a","i_b"]
            [BindValue
              "a"
              (ArithmeticExp (
                Variable (Value "i_a"))),
            BindValue
              "b"
              (ArithmeticExp (
                Variable (Value "i_b")))]
            [FunctionWithParam
              "add"
              "c"
              (FunctionEval (
                FunctionCall (
                  FunctionEval (
                    FunctionCall
                      (ValueOf (Value "Complex"))
                      (ArithmeticExp (
                        ArithmeticOperation
                          Add
                          (Variable (Value "a"))
                          (Variable (DomainValue "c" "a"))))))
                  (ArithmeticExp (
                    ArithmeticOperation
                      Add
                      (Variable (Value "b"))
                      (Variable (DomainValue "c" "b")))))),
            FunctionWithParam
              "multiply_s"
                "s"
                (FunctionEval (
                  FunctionCall (
                    FunctionEval (
                      FunctionCall
                        (ValueOf (Value "Complex"))
                        (ArithmeticExp (
                          ArithmeticOperation
                            Multiply
                            (Variable (Value "a"))
                            (Variable (Value "s"))))))
                  (ArithmeticExp (
                    ArithmeticOperation
                      Multiply
                      (Variable (Value "b"))
                      (Variable (Value "s"))))))])))])


testAssertions :: [(String, IO Bool)] -> [IO String]
testAssertions = map statusStrings
  where statusStrings (name, iores) = do
          result <- iores
          return $ if result then "passed" else "failed: " ++ name

printTests :: [IO String] -> IO ()
printTests ioResults = do
  results <- sequence ioResults
  print results

runTests = printTests $
  testAssertions [
    ("simple-arith.dom", assertSimpleArith),
    ("simple-bool.dom", assertSimpleBool),
    ("simple-binding.dom", assertSimpleBinding),
    ("simple-function.dom", assertSimpleFunction),
    ("simple-application.dom", assertSimpleApplication),
    ("simple-domain.dom", assertSimpleDomain)]
