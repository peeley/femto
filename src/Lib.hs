module Lib (
    eval,
    parse,
    repl,
    defaultEnv,
    newIORef
) where

import ParserType
import Errors
import Parser
import Lexer
import Eval
import qualified Data.Map.Strict as M
import System.IO
import Data.IORef


defaultFuncs :: DefaultFuncs
defaultFuncs =  M.fromList 
                [("+", \[Integer x, Integer y] -> Right $ Integer (x + y)),
                 ("-", \[Integer x, Integer y] -> Right $ Integer (x - y)),
                 ("*", \[Integer x, Integer y] -> Right $ Integer (x * y)),
                 ("/", \[Integer x, Integer y] -> Right $ Integer (x `div` y)),
                 ("^", \[Integer x, Integer y] -> Right $ Integer (x ^ y)),
                 ("inc", \[Integer x] -> Right $ Integer (x+1)),
                 ("dec", \[Integer x] -> Right $ Integer (x-1)),
                 ("=", \[x, y] -> Right $ Boolean (x == y)),
                 ("!=", \[x, y] -> Right $ Boolean (x /= y)),
                 ("<", \[Integer x, Integer y] -> Right $ Boolean (x < y)),
                 (">", \[Integer x, Integer y] -> Right $ Boolean (x > y)),
                 ("<=", \[Integer x, Integer y] -> Right $ Boolean (x <= y)),
                 (">=", \[Integer x, Integer y] -> Right $ Boolean (x >= y)),
                 ("&&", \[Boolean x, Boolean y] -> Right $ Boolean (x && y)),
                 ("||", \[Boolean x, Boolean y] -> Right $ Boolean (x || y)),
                 ("not", \[Boolean x] -> Right $ Boolean (not x)),
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

car :: [LispVal] -> EvalResult
car [List (x:xs)] = return x
car l = Left $ TypeError (show l) "nonempty list"

cdr :: [LispVal] -> EvalResult
cdr [List (x:xs)] = return $ List xs
cdr l = Left $ TypeError (show l) "nonempty list"

cons :: [LispVal] -> EvalResult
cons [List x, List y] = return $ List (x++y)
cons [x, List y] = return $ List (x:y)
cons x = Left $ TypeError (show x) "two lists or singleton and list"

isNull :: [LispVal] -> EvalResult
isNull [List l] = if null l then Right (Boolean True) else Right (Boolean False)
isNull x = Left $ TypeError (show x) "list"

repl :: Environment -> IO ()
repl env = do
    putStr "> "
    hFlush stdout
    input <- getLine
    let ast = parse input
    out <- eval env ast
    case out of
        Left err -> print err >> repl env
        Right result -> print result >> repl env
