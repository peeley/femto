module Eval where

import Parser
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe

type Environment = M.Map String ([LispVal] -> LispVal) -- map words to functions

eval :: IORef Environment -> LispVal -> IO LispVal
eval _ (List [Word "quote", val]) = return val
eval env (List [Word "define", Word name, body]) = do 
    oldEnv <- readIORef env
    writeIORef env (M.insert name (const body) oldEnv)
    return $ Word name
eval env (Word word) = do
    m_env <- readIORef env
    let def = lookup word m_env
    case def of
        Just x -> return $ x ()
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
