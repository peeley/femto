module Lib ( 
repl
) where

import ParserType
import Parser
import Lexer
import Eval
import qualified Data.Map.Strict as M

defaultEnv :: Environment
defaultEnv = M.fromList [("+", \[Integer x, Integer y] -> Integer (x + y)),
                         ("-", \[Integer x, Integer y] -> Integer (x - y))]

repl :: IO ()
repl = do
    input <- getLine
    let ast = parse input
    let out = eval defaultEnv ast
    print out
    repl
