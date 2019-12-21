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


defaultFuncs :: DefaultFuncs
defaultFuncs =  M.fromList 
                [("+", \[Integer x, Integer y] -> Integer (x + y)),
                 ("-", \[Integer x, Integer y] -> Integer (x - y)),
                 ("*", \[Integer x, Integer y] -> Integer (x * y)),
                 ("/", \[Integer x, Integer y] -> Integer (x `div` y)),
                 ("^", \[Integer x, Integer y] -> Integer (x ^ y)),
                 ("inc", \[Integer x] -> Integer (x+1)),
                 ("dec", \[Integer x] -> Integer (x-1)),
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
                 ("head", car),
                 ("cdr", cdr),
                 ("tail", cdr),
                 ("cons", cons),
                 ("null?", isNull)]

defaultEnv :: IO Environment
defaultEnv = do
    vars <- newIORef M.empty
    funcs <- newIORef M.empty
    return $ Environment { vars = vars, funcs = funcs, defaults = defaultFuncs }

car :: [LispVal] -> LispVal
car [List (x:xs)] = x
car [List []] = error "Empty list"
car _ = error "Error: car expects nonempty list."

cdr :: [LispVal] -> LispVal
cdr [List (x:xs)] = List xs
cdr _ = error "Error: cdr expects nonempty list."

cons :: [LispVal] -> LispVal
cons [List x, List y] = List (x++y)
cons [x, List y] = List (x:y)
cons _ = error "Error: cons expects two lists, or value and list"

isNull :: [LispVal] -> LispVal
isNull [List l] = if null l then Boolean True else Boolean False
isNull _ = error "Error: empty? expects list"

repl :: Environment -> IO ()
repl env = do
    putStr "> "
    hFlush stdout
    input <- getLine
    let ast = parse input
    out <- eval env ast
    print out
    repl env
