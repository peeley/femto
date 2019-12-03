module Main where

import Lib
import qualified System.Environment as E
import Control.Monad

main :: IO ()
main = do
    putStrLn ""
    env <- newIORef defaultEnv
    args <- E.getArgs
    when (null args) $ repl env
    program <- readFile $ head args
    let ast = parse program
    result <- eval env ast
    return ()
