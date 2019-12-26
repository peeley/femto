import Test.HUnit
import Parser
import Eval
import Lib
import Control.Monad (void)
import Control.Monad.IO.Class

main :: IO ()
main = void $ runTestTT testSuite

testSuite = TestList [TestLabel "Parse expressions" testParseExpr,
                      TestLabel "Evaluate arithmetic" testEvalArithmetic]

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
    assertEqual "" result (Right $ Integer 3)
    result <- eval env $ parse "(- 1 1)"
    assertEqual "" result (Right $ Integer 0)
    result <- eval env $ parse "(* 3 3)"
    assertEqual "" result (Right $ Integer 9)
    result <- eval env $ parse "(/ 4 2)"
    assertEqual "" result (Right $ Integer 2)

