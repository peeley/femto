module Eval where

import Parser
import Errors
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Either
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
type DefaultFuncs = M.Map String ([LispVal] -> EvalResult)
data Environment = Environment { 
                        vars :: IORef Vars, 
                        funcs :: IORef Funcs,
                        defaults :: DefaultFuncs }

eval :: Environment -> LispVal -> IO EvalResult
eval _ (List [Word "quote", val]) = return $ return val
eval env (List [Word "eval", val]) = eval env val
eval env (List [Word "define", Word name, body]) = do
    evalBody <- eval env body
    case evalBody of
        Right evalRes -> do
            oldVars <- readIORef $ vars env
            writeIORef (vars env) (M.insert name evalRes oldVars)
            return $ Right $ Word name
        Left err -> return $ Left err
eval env (List [Word "define", List (Word name:args), body]) = do
    oldFuncs <- readIORef $ funcs env
    vars <- readIORef $ vars env
    let func = LispFunc { args = map show args, body = body, closure = vars }
    writeIORef (funcs env) (M.insert name func oldFuncs)
    return $ Right $ Word name
eval env (List [Word "print", val]) = do
    eval env val >>= print
    return $ Right $ List []
eval env (List [Word "if", cond, t, f]) = do
    evalCond <- eval env cond
    case evalCond of
        Right (Boolean True) -> eval env t
        Right (Boolean False) -> eval env f
        _ -> return $ Left $ TypeError (show cond) "bool"
eval env (List (Word "do":rest)) = last <$> mapM (eval env) rest
eval env word@(Word name) = do
    variables <- readIORef $ vars env
    let varDef = M.lookup name variables
    case varDef of
        Just x -> return $ Right x
        _ -> return $ Right word
eval env (List (Word fun : args)) = do 
    argList <- mapM (eval env) args
    if (length . rights) argList == length args then
        apply env fun $ rights argList
    else
        (return . Left . head . lefts) argList
eval env val@(List [x]) = return $ Left $ Misc $ show x ++ " is not procedure."
eval env val@(Integer i) = return $ Right val
eval env val@(Boolean b) = return $ Right val
eval env val@(String s) = return $ Right val

apply :: Environment -> String -> [LispVal] -> IO EvalResult
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
                    _ -> return $ Left $ NotFunc fname
            else return $ fromJust primitive argVals
        else do
            let lispFunc = fromJust m_lispFunc
            let funcArgs = args lispFunc
            if length argVals /= length funcArgs then 
                return $ Left $ NumArgs fname (length funcArgs) (length argVals)
            else do
                let argNames = args lispFunc
                let params = zip argNames argVals
                outerScope <- readIORef $ vars env
                funcVars <- newIORef $ M.union (M.fromList params) outerScope
                let funcEnv = Environment { vars = funcVars, 
                                            funcs = funcs env,
                                            defaults = defaults env}
                eval funcEnv $ body lispFunc
