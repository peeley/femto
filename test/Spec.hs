import Test.HUnit
import Parser
import Eval
import Lib
import LispTypes
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Either

main :: IO ()
main = void $ runTestTT testSuite

testSuite = TestList [TestLabel "Parse expressions" testParseExpr,
                      TestLabel "Evaluate arithmetic" testEvalArithmetic,
                      TestLabel "Evaluate boolean logic" testEvalBoolean,
                      TestLabel "List functions" testListFuncs,
                      TestLabel "Define variables" testDefine,
                      TestLabel "Pass/define values" testLambda,
                      TestLabel "Apply functions" testApply,
                      TestLabel "Standard library" testLoad]

runProgram :: Environment -> (String -> IO EvalResult)
runProgram env = fmap last . mapM (eval env) . parse

testParseExpr = TestList [
    parse "()" ~=? [List []],
    parse "123" ~=? [Integer 123],
    parse "#t" ~=? [Boolean True],
    parse "\"abc\"" ~=? [String "abc"],
    parse "(abc)" ~=? [List [Word "abc"]],
    parse "(+ 1 2)" ~=? [List [Word "+", Integer 1, Integer 2]]]

testEvalArithmetic = TestCase $ do
    env <- defaultEnv
    result <- runProgram env "(+ 1 2)"
    assertEqual "Addition" result (Right $ Integer 3)
    result <- runProgram env "(- 1 1)"
    assertEqual "Subtraction" result (Right $ Integer 0)
    result <- runProgram env "(* 3 3)"
    assertEqual "Multiplication" result (Right $ Integer 9)
    result <- runProgram env "(/ 4 2)"
    assertEqual "Division" result (Right $ Integer 2)
    result <- runProgram env "(^ 2 2)"
    assertEqual "Power" result (Right $ Integer 4)
    result <- runProgram env "(/ 1 0)"
    assertBool "Divide by zero" (isLeft result)

testEvalBoolean = TestCase $ do
    env <- defaultEnv
    result <- runProgram env "(= 1 1)"
    assertEqual "Equals" result (Right $ Boolean True)
    result <- runProgram env "(!= 1 2)"
    assertEqual "Not equals" result (Right $ Boolean True)
    result <- runProgram env "(< 1 2)"
    assertEqual "Less than" result (Right $ Boolean True)
    result <- runProgram env "(&& #t #t)"
    assertEqual "And" result (Right $ Boolean True)
    result <- runProgram env "(|| #t #f)"
    assertEqual "Or" result (Right $ Boolean True)
    result <- runProgram env "(not #f)"
    assertEqual "Not" result (Right $ Boolean True)

testListFuncs = TestCase $ do
    env <- defaultEnv
    result <- runProgram env "(head '(1 2 3))"
    assertEqual "Head/car" result (Right $ Integer 1)
    result <- runProgram env "(tail '(1 2 3))"
    assertEqual "Tail/cdr" result (Right $ List [Integer 2, Integer 3])
    result <- runProgram env "(cons 1 '(2 3))"
    assertEqual "Cons singleton and list" result 
        (Right $ List [Integer 1, Integer 2, Integer 3])
    result <- runProgram env "(cons '(1 2) '(3 4))"
    assertEqual "Cons two lists" result 
        (Right $ List [Integer 1, Integer 2, Integer 3, Integer 4])
    result <- runProgram env "(cons '(1 2 3) '())"
    assertEqual "Cons list and empty list" result 
        (Right $ List [Integer 1, Integer 2, Integer 3])
    result <- runProgram env "(head '())"
    assertBool "Head/car fails on empty" (isLeft result)
    result <- runProgram env "(tail '())"
    assertBool "Tail/cdr fails on empty" (isLeft result)

testDefine = TestCase $ do
    env <- defaultEnv
    result <- runProgram env "(define x 5) x"
    assertEqual "Define integer variable" result (Right $ Integer 5)
    result <- runProgram env "(define (square x) (* x x)) (square 3)"
    assertEqual "Define simple procedure" result (Right $ Integer 9)
    result <- runProgram env "(define (factorial n) \
              \     (if (= n 0) \
              \         1 \
              \         (* n (factorial (- n 1))))) \
              \    (factorial 5)"
    assertEqual "Recursive procedures" result (Right $ Integer 120)

testLambda = TestCase $ do
    env <- defaultEnv
    result <- runProgram env "(define square (lambda (x) (* x x))) \
                             \(square 5)"
    assertEqual "Define var as lambda" result (Right $ Integer 25)
    result <- runProgram env "(define add (lambda (x y) (+ x y))) \
                             \(add 1 2)"
    assertEqual "Define var as lambda, multiple args" result (Right $ Integer 3)

testApply = TestCase $ do
    env <- defaultEnv
    result <- runProgram env "(define (square x) (* x x)) \
                             \(apply square '(3))"
    assertEqual "Apply bound function" result (Right $ Integer 9)
    result <- runProgram env "(apply (lambda (x) (* x x)) '(3))"
    assertEqual "Apply lambda function" result (Right $ Integer 9)

testLoad = TestCase $ do
    env <- defaultEnv
    result <- runProgram env "(load \"src/stdlib.fm\") (square 5)"
    assertEqual "Load func from stdlib" result (Right $ Integer 25)
    result <- runProgram env "(map inc '(1 2 3))"
    assertEqual "Map function" result 
        (Right $ List [Integer 2, Integer 3, Integer 4])
    result <- runProgram env "(fold + 0 '(1 2 3))"
    assertEqual "Fold function" result (Right $ Integer 6)
