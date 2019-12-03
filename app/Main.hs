module Main where

import Lib
import qualified System.Environment as E

main :: IO ()
main = do
    putStrLn ""
    env <- newIORef defaultEnv
    args <- E.getArgs
    when (null args) $ repl env
    program <- readFile $ head E.getArgs
    eval env $ parse program
