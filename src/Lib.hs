module Lib ( 
repl
) where

import ParserType
import Parser
import Lexer
import Eval
import qualified Data.Map.Strict as M
import System.IO

defaultEnv :: Environment
defaultEnv = M.fromList [("+", \[Integer x, Integer y] -> Integer (x + y)),
                         ("-", \[Integer x, Integer y] -> Integer (x - y)),
                         ("*", \[Integer x, Integer y] -> Integer (x * y)),
                         ("/", \[Integer x, Integer y] -> Integer (x `div` y)),
                         ("^", \[Integer x, Integer y] -> Integer (x ^ y)),
                         ("=", \[x, y] -> Boolean (x==y)),
                         ("!=", \[x, y] -> Boolean (x/=y))]

repl :: IO ()
repl = do
    putStr "> "
    hFlush stdout
    input <- getLine
    let ast = parse input
    let out = eval defaultEnv ast
    print out
    repl
