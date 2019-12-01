module Main where

import Lib

main :: IO ()
main = do
    putStrLn ""
    env <- newIORef defaultEnv
    repl env
