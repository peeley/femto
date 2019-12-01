module Eval where

import Parser
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe

type Environment = M.Map String ([LispVal] -> LispVal) -- map words to functions

eval :: IORef Environment -> LispVal -> IO LispVal
eval _ (List [Word "quote", val]) = return val
eval env (List [Word "define", Word name, body]) = do 
    evalBody <- eval env body
    oldEnv <- readIORef env
    writeIORef env (M.insert name (const evalBody) oldEnv)
    return $ Word name
eval env (List [Word "print", val]) = do
    eval env val >>= print
    return $ List []
eval env (List [Word "if", cond, t, f]) = do
    evalCond <- eval env cond
    case evalCond of
        Boolean True -> eval env t
        Boolean False -> eval env f
        _ -> error "Expected boolean in if statement"
eval env (Word word) = do
    m_env <- readIORef env
    let def = M.lookup word m_env
    case def of
        Just x -> return $ x [] -- apply const function to get x
        Nothing -> error "Word not defined"
eval env (List (Word fun : args)) = do
    m_args <- mapM (eval env) args
    apply env fun m_args
eval _ val = return val

apply :: IORef Environment -> String -> [LispVal] -> IO LispVal
apply env fun args = do
    m_env <- readIORef env
    case M.lookup fun m_env of
        Just f -> return $ f args
        Nothing -> error "Word not found."
