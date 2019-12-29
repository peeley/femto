module Eval where

import LispTypes
import Parser
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Either
import Control.Monad (when)

eval :: Environment -> LispVal -> IO EvalResult
eval _ (List [Word "quote", val]) = return $ return val
eval env (List [Word "eval", val]) = do
    body <- eval env val
    case body of
        Left err -> return $ Left err
        Right list -> eval env list
eval env (List [Word "define", Word name, body]) = do
    evalBody <- eval env body
    case evalBody of
        Right evalRes -> do
            oldbindings <- readIORef env
            writeIORef env (M.insert name evalRes oldbindings)
            return $ Right $ Word name
        Left err -> return $ Left err
eval env (List [Word "define", List (Word name:args), body]) = do
    oldbindings <- readIORef env
    let func = Function { args = map show args, body = body, closure = oldbindings}
    writeIORef env (M.insert name func oldbindings)
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
    variables <- readIORef env
    let varDef = lookup name variables
    case varDef of
        --Just (Func{}) -> return $ Left $ TypeError name "variable"
        --Just (DefaultFunc _) -> return $ Left $ TypeError name "variable"
        Just x -> return $ Right x
        Nothing -> return $ Left $ Undefined name
eval env (List [Word "load", String filename]) = do
    fileContents <- readFile filename
    let loadedAst = parse fileContents
    eval env loadedAst
    return $ Right $ List []
eval env (List (Word fun : args)) = do 
    argList <- mapM (eval env) args
    if (length . rights) argList == length args then
        apply env fun $ rights argList -- if all args evaluate, pass to apply
    else
        (return . Left . head . lefts) argList -- otherwise, return error
eval env val@(List [x]) = return $ Left $ NotFunc $ show x
eval env val@(Integer i) = return $ Right val
eval env val@(Boolean b) = return $ Right val
eval env val@(String s) = return $ Right val

apply :: Environment -> String -> [LispVal] -> IO EvalResult
apply env fname argVals = do
    bindings <- readIORef env
    let boundDef = lookup fname bindings
    case boundDef of
        Just (DefaultFunc f) -> return $ Right $ f argVals
        Just Function{args = args, body = body, closure = closure} ->
            if length args /= length argVals then
                return $ Left $ NumArgs fname (length args) (length argVals)
            else do
                let params = zip args argVals
                outerScope <- readIORef env
                innerScope <- newIORef $ M.union (M.fromList params) outerScope
                eval innerScope body
        Just x -> return $ Left $ NotFunc fname
        Nothing -> return $ Left $ Undefined fname
    {--
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
                eval funcEnv $ body lispFunc --}
