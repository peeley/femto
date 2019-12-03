module Lib (
    eval,
    parse,
    repl,
    defaultEnv,
    newIORef
) where

import ParserType
import Parser
import Lexer
import Eval
import qualified Data.Map.Strict as M
import System.IO
import Data.IORef

defaultEnv :: Environment
defaultEnv = M.fromList [("+", \[Integer x, Integer y] -> Integer (x + y)),
                         ("-", \[Integer x, Integer y] -> Integer (x - y)),
                         ("*", \[Integer x, Integer y] -> Integer (x * y)),
                         ("/", \[Integer x, Integer y] -> Integer (x `div` y)),
                         ("^", \[Integer x, Integer y] -> Integer (x ^ y)),
                         ("=", \[x, y] -> Boolean (x == y)),
                         ("!=", \[x, y] -> Boolean (x /= y)),
                         ("<", \[Integer x, Integer y] -> Boolean (x < y)),
                         (">", \[Integer x, Integer y] -> Boolean (x > y)),
                         ("<=", \[Integer x, Integer y] -> Boolean (x <= y)),
                         (">=", \[Integer x, Integer y] -> Boolean (x >= y)),
                         ("&&", \[Boolean x, Boolean y] -> Boolean (x && y)),
                         ("||", \[Boolean x, Boolean y] -> Boolean (x || y)),
                         ("not", \[Boolean x] -> Boolean (not x)),
                         ("car", car),
                         ("cdr", cdr),
                         ("cons", cons)]

car :: [LispVal] -> LispVal
car [List (x:xs)] = List [x]
car [List []] = error "Empty list"
car _ = error "Arg not valid list"

cdr :: [LispVal] -> LispVal
cdr [List (x:xs)] = List xs
cdr [List []] = error "Empty list"
cdr _ = error "Arg not valid list"

cons :: [LispVal] -> LispVal
cons [List x, List y] = List (x++y)
cons [x, List y] = List (x:y)
cons _ = error "Unable to combine args"

repl :: IORef Environment -> IO ()
repl env = do
    putStr "> "
    hFlush stdout
    input <- getLine
    let ast = parse input
    out <- eval env ast
    print out
    repl env
