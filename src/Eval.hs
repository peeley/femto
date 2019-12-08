module Eval where

import Parser
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe

type Vars = M.Map String LispVal -- map words to variables
type Funs = M.Map String ([LispVal] -> LispVal)
data Environment = Environment { vars :: IORef Vars, funs :: IORef Funs }

eval :: Environment -> LispVal -> IO LispVal
eval _ (List [Word "quote", val]) = return val
eval env (List [Word "define", Word name, body]) = do 
    evalBody <- eval env body
    oldVars <- readIORef $ vars env
    writeIORef (vars env) (M.insert name evalBody oldVars)
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
eval env (List [Word "do", List list]) = do
    seqnce <-  mapM (eval env) list
    return $ last seqnce
eval env (Word word) = do
    m_env <- readIORef $ vars env
    let def = M.lookup word m_env
    case def of
        Just x -> return x
        Nothing -> error "Word not defined"
eval env (List (Word fun : args)) = do
    m_args <- mapM (eval env) args
    apply env fun m_args
eval _ val = return val

apply :: Environment -> String -> [LispVal] -> IO LispVal
apply env fun args = do
    m_env <- readIORef $ funs env
    case M.lookup fun m_env of
        Just f -> return $ f args
        Nothing -> error "Word is not a procedure."
