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
                      TestLabel "Define variables" testDefine]

testParseExpr = TestList [
    parse "()" ~=? List [],
    parse "123" ~=? Integer 123,
    parse "#t" ~=? Boolean True,
    parse "\"abc\"" ~=? String "abc",
    parse "(abc)" ~=? List [Word "abc"],
    parse "(+ 1 2)" ~=? List [Word "+", Integer 1, Integer 2]]

testEvalArithmetic = TestCase $ do
    env <- defaultEnv
    result <- eval env $ parse "(+ 1 2)"
    assertEqual "Addition" result (Right $ Integer 3)
    result <- eval env $ parse "(- 1 1)"
    assertEqual "Subtraction" result (Right $ Integer 0)
    result <- eval env $ parse "(* 3 3)"
    assertEqual "Multiplication" result (Right $ Integer 9)
    result <- eval env $ parse "(/ 4 2)"
    assertEqual "Division" result (Right $ Integer 2)
    result <- eval env $ parse "(^ 2 2)"
    assertEqual "Power" result (Right $ Integer 4)
    result <- eval env $ parse "(/ 1 0)"
    assertBool "Divide by zero" (isLeft result)

testEvalBoolean = TestCase $ do
    env <- defaultEnv
    result <- eval env $ parse "(= 1 1)"
    assertEqual "Equals" result (Right $ Boolean True)
    result <- eval env $ parse "(!= 1 2)"
    assertEqual "Not equals" result (Right $ Boolean True)
    result <- eval env $ parse "(< 1 2)"
    assertEqual "Less than" result (Right $ Boolean True)
    result <- eval env $ parse "(&& #t #t)"
    assertEqual "And" result (Right $ Boolean True)
    result <- eval env $ parse "(|| #t #f)"
    assertEqual "Or" result (Right $ Boolean True)
    result <- eval env $ parse "(not #f)"
    assertEqual "Not" result (Right $ Boolean True)

testListFuncs = TestCase $ do
    env <- defaultEnv
    result <- eval env $ parse "(head '(1 2 3))"
    assertEqual "Head/car" result (Right $ Integer 1)
    result <- eval env $ parse "(tail '(1 2 3))"
    assertEqual "Tail/cdr" result (Right $ List [Integer 2, Integer 3])
    result <- eval env $ parse "(cons 1 '(2 3))"
    assertEqual "Cons singleton and list" result 
        (Right $ List [Integer 1, Integer 2, Integer 3])
    result <- eval env $ parse "(cons '(1 2) '(3 4))"
    assertEqual "Cons two lists" result 
        (Right $ List [Integer 1, Integer 2, Integer 3, Integer 4])
    result <- eval env $ parse "(cons '(1 2 3) '())"
    assertEqual "Cons list and empty list" result 
        (Right $ List [Integer 1, Integer 2, Integer 3])
    result <- eval env $ parse "(head '())"
    assertBool "Head/car fails on empty" (isLeft result)
    result <- eval env $ parse "(tail '())"
    assertBool "Tail/cdr fails on empty" (isLeft result)

testDefine = TestCase $ do
    env <- defaultEnv
    result <- eval env $ parse "(do (define x 5) x)"
    assertEqual "Define integer variable" result (Right $ Integer 5)
    result <- eval env $ 
        parse "(do (define (square x) (* x x)) (square 3))"
    assertEqual "Define simple procedure" result (Right $ Integer 9)
    result <- eval env $
        parse "(do (define (factorial n) \
              \     (if (= n 0) \
              \         1 \
              \         (* n (factorial (- n 1))))) \
              \    (factorial 5))"
    assertEqual "Recursive procedures" result (Right $ Integer 120)

