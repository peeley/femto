module Lib (
    eval,
    parse,
    repl,
    defaultEnv,
    newIORef,
    LispVal
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
                [("+", add), ("-", sub), ("*", mult), ("/", div_), ("^", pow),
                 ("inc", inc), ("dec", dec),
                 ("=", \[x, y] -> return $ Boolean (x == y)),
                 ("!=", \[x, y] -> return $ Boolean (x /= y)),
                 ("<", lt), (">", gt), ("<=", lte), (">=", gte), ("&&", and_),
                 ("||", or_), ("not", not_), ("car", car), ("head", car),
                 ("cdr", cdr), ("tail", cdr), ("cons", cons)]

defaultEnv :: IO Environment
defaultEnv = do
    vars <- newIORef M.empty
    funcs <- newIORef M.empty
    return $ Environment { vars = vars, funcs = funcs, defaults = defaultFuncs }

add :: [LispVal] -> EvalResult
add [Integer x, Integer y] = return $ Integer (x+y)
add [Integer x, y] = Left $ TypeError (show y) "integer"
add [x, Integer y] = Left $ TypeError (show x) "integer"
add _ = Left $ TypeError "operands" "integer"

sub :: [LispVal] -> EvalResult
sub [Integer x, Integer y] = return $ Integer (x-y)
sub [Integer x, y] = Left $ TypeError (show y) "integer"
sub [x, Integer y] = Left $ TypeError (show x) "integer"
sub _ = Left $ TypeError "operands" "integer"

mult :: [LispVal] -> EvalResult
mult [Integer x, Integer y] = return $ Integer (x*y)
mult [Integer x, y] = Left $ TypeError (show y) "integer"
mult [x, Integer y] = Left $ TypeError (show x) "integer"
mult _ = Left $ TypeError "operands" "integer"

div_ :: [LispVal] -> EvalResult
div_ [Integer x, Integer 0] = Left $ Misc "Cannot divide by zero."
div_ [Integer x, Integer y] = return $ Integer (x `div` y)
div_ [Integer x, y] = Left $ TypeError (show y) "integer"
div_ [x, Integer y] = Left $ TypeError (show x) "integer"
div_ _ = Left $ TypeError "operands" "integer"

pow :: [LispVal] -> EvalResult
pow [Integer x, Integer y] = return $ Integer (x^y)
pow [Integer x, y] = Left $ TypeError (show y) "integer"
pow [x, Integer y] = Left $ TypeError (show x) "integer"
pow _ = Left $ TypeError "operands" "integer"

lt :: [LispVal] -> EvalResult
lt [Integer x, Integer y] = return $ Boolean (x < y)
lt [Integer x, y] = Left $ TypeError (show y) "integer"
lt [x, Integer y] = Left $ TypeError (show x) "integer"
lt _ = Left $ TypeError "operands" "integer"

gt :: [LispVal] -> EvalResult
gt [Integer x, Integer y] = return $ Boolean (x > y)
gt [Integer x, y] = Left $ TypeError (show y) "integer"
gt [x, Integer y] = Left $ TypeError (show x) "integer"
gt _ = Left $ TypeError "operands" "integer"

lte :: [LispVal] -> EvalResult
lte [Integer x, Integer y] = return $ Boolean (x <= y)
lte [Integer x, y] = Left $ TypeError (show y) "integer"
lte [x, Integer y] = Left $ TypeError (show x) "integer"
lte _ = Left $ TypeError "operands" "integer"

gte :: [LispVal] -> EvalResult
gte [Integer x, Integer y] = return $ Boolean (x >= y)
gte [Integer x, y] = Left $ TypeError (show y) "integer"
gte [x, Integer y] = Left $ TypeError (show x) "integer"
gte _ = Left $ TypeError "operands" "integer"

and_ :: [LispVal] -> EvalResult
and_ [Boolean x, Boolean y] = return $ Boolean (x && y)
and_ [Integer x, y] = Left $ TypeError (show y) "boolean"
and_ [x, Integer y] = Left $ TypeError (show x) "boolean"
and_ _ = Left $ TypeError "operands" "boolean"

or_ :: [LispVal] -> EvalResult
or_ [Boolean x, Boolean y] = return $ Boolean (x || y)
or_ [Integer x, y] = Left $ TypeError (show y) "boolean"
or_ [x, Integer y] = Left $ TypeError (show x) "boolean"
or_ _ = Left $ TypeError "operands" "boolean"

inc :: [LispVal] -> EvalResult
inc [Integer x] = return $ Integer (x+1)
inc x = Left $ TypeError (show x) "integer"

dec :: [LispVal] -> EvalResult
dec [Integer x] = return $ Integer (x-1)
dec x = Left $ TypeError (show x) "integer"

not_ :: [LispVal] -> EvalResult
not_ [Boolean x] = return $ Boolean (not x)
not_ x = Left $ TypeError (show x) "boolean"

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
