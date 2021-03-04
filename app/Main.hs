module Main where

import Lib
import qualified System.Environment as E
import Control.Monad

main :: IO ()
main = do
    args <- E.getArgs
    env <- defaultEnv
    if null args then do
        putStrLn "\nWelcome to Femto!\n"
        repl env
    else do
        putStrLn ""
        when (null args) $ repl env
        program <- readFile $ head args
        let ast = parse program
        result <- last <$> mapM (eval env) ast
        return ()
