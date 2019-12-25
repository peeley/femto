import Test.HUnit
import Parser
import Eval
import Control.Monad (void)

main :: IO ()
main = void $ runTestTT testSuite

testSuite = TestList [TestLabel "Parse expressions" testParseExpr]

testParseExpr = TestList [parse "()" ~=? List [],
    parse "123" ~=? Integer 123,
    parse "#t" ~=? Boolean True,
    parse "\"abc\"" ~=? String "abc",
    parse "(abc)" ~=? List [Word "abc"],
    parse "(+ 1 2)" ~=? List [Word "+", Integer 1, Integer 2]]
