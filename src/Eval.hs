module Eval where

import Parser
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import Control.Monad (when)

type Vars = M.Map String LispVal -- map words to variables
data LispFunc = LispFunc { 
                    args :: [String],
                    body :: LispVal,
                    closure :: Vars
                    }
instance Eq LispFunc where
    a == b = body a == body b
instance Ord LispFunc where
    a <= b = body a <= body b
type Funcs = M.Map String LispFunc
newtype InterpError = InterpError String
type DefaultFuncs = M.Map String ([LispVal] -> LispVal)
data Environment = Environment { 
                        vars :: IORef Vars, 
                        funcs :: IORef Funcs,
                        defaults :: DefaultFuncs }

eval :: Environment -> LispVal -> IO LispVal
eval _ (List [Word "quote", val]) = return val
eval env (List [Word "eval", val]) = eval env val >>= eval env
eval env (List [Word "define", Word name, body]) = do 
    evalBody <- eval env body
    oldVars <- readIORef $ vars env
    writeIORef (vars env) (M.insert name evalBody oldVars)
    return $ Word name
eval env (List [Word "define", List (Word name:args), body]) = do
    oldFuncs <- readIORef $ funcs env
    vars <- readIORef $ vars env
    let func = LispFunc { args = map show args, body = body, closure = vars }
    writeIORef (funcs env) (M.insert name func oldFuncs)
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
eval env (List [Word "do", List list]) = last <$> mapM (eval env) list
eval env word@(Word name) = do
    variables <- readIORef $ vars env
    let varDef = M.lookup name variables
    case varDef of
        Just x -> return x
        _ -> return word
eval env (List (Word fun : args)) = mapM (eval env) args >>= apply env fun 
eval env val@(Integer i) = return val
eval env val@(Boolean b) = return val
eval env val@(String s) = return val
eval _ val = error $ "Unable to evaluate value " ++ show val

apply :: Environment -> String -> [LispVal] -> IO LispVal
apply env fname argVals = do
    envFuncs <- readIORef $ funcs env
    let m_lispFunc =  M.lookup fname envFuncs
    if isNothing m_lispFunc
        then do
            let primitive = M.lookup fname $ defaults env
            if isNothing primitive then do
                envVars <- readIORef $ vars env
                let passedFunc = M.lookup fname envVars
                case passedFunc of
                    Just (Word f) -> apply env f argVals
                    Nothing -> error $ "Word " ++ fname ++ " is not procedure."
            else return $ fromJust primitive argVals
        else do
            let lispFunc = fromJust m_lispFunc
            let funcArgs = args lispFunc
            when (length argVals /= length funcArgs) 
                $ error $ "Expected "++(show . length) funcArgs ++", received "
                        ++ (show . length) argVals ++ " arguments."
            let argNames = args lispFunc
            let params = zip argNames argVals
            outerScope <- readIORef $ vars env
            funcVars <- newIORef $ M.union (M.fromList params) outerScope
            let funcEnv = Environment { vars = funcVars, 
                                        funcs = funcs env,
                                        defaults = defaults env}
            eval funcEnv $ body lispFunc
