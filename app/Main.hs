module Main where

import Lib
import Codegen
import qualified System.Environment as E
import Control.Monad

main :: IO ()
main = do
    args <- E.getArgs
    if length args > 1 then do
        let ast = runLLVM initModule logic
        runJit ast
        return ()
    else do
        putStrLn ""
        env <- defaultEnv
        when (null args) $ repl env
        program <- readFile $ head args
        let ast = parse program
        result <- last <$> mapM (eval env) ast
        return ()
