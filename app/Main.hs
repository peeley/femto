module Main where

import Lib
import Codegen
import qualified System.Environment as E
import Control.Monad

main :: IO ()
main = do
    args <- E.getArgs
    env <- defaultEnv
    when (null args) $ repl env
    if head args == "-c" then
        toLLVM initModule
    else do
        putStrLn ""
        when (null args) $ repl env
        program <- readFile $ head args
        let ast = parse program
        result <- last <$> mapM (eval env) ast
        return ()
