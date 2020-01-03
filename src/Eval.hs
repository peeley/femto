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
    let func = Function { args = map show args, 
                          body = body, 
                          closure = oldbindings}
    writeIORef env (M.insert name func oldbindings)
    return $ Right $ Word name
eval env (List [Word "lambda", List args, body@(List _)]) = do
    thisEnv <- readIORef env
    return $ Right $ Function { args = map show args, 
                                body = body, 
                                closure = thisEnv }
eval env (List [Word "print", val]) = do
    evaled <- eval env val
    case evaled of
        Right val -> print val >> return (Right $ List [])
        Left l -> return $ Left l
eval env (List [Word "if", cond, t, f]) = do
    evalCond <- eval env cond
    case evalCond of
        Right (Boolean True) -> eval env t
        Right (Boolean False) -> eval env f
        _ -> return $ Left $ TypeError (show cond) "bool"
eval env (List (Word "do":rest)) = last <$> mapM (eval env) rest
eval env word@(Word name) = do
    variables <- readIORef env
    let varDef = M.lookup name variables
    case varDef of
        Just x -> return $ Right x
        Nothing -> return $ Left $ Undefined name
eval env (List [Word "load", String filename]) = do
    fileContents <- readFile filename
    let loadedAst = parse fileContents
    eval env loadedAst
    return $ Right $ List []
eval env (List [Word "apply", func, funcargs]) = do
    evaledArgs <- eval env funcargs
    case evaledArgs of
        Left l -> return $ Left l
        Right (List args) -> do
            evaledFunc <- eval env func
            bindings <- readIORef env
            case evaledFunc of
                Right f@Function{} -> evaluateArgs env f args
                Left l -> return $ Left l
                Right (Word w) -> do
                    let lookedup = M.lookup w bindings
                    case lookedup of 
                        Just f@Function{} -> evaluateArgs env f args
                        Just f@(DefaultFunc _) -> evaluateArgs env f args
                        Just x -> return $ Left $ NotFunc (show x)
                        Nothing -> return $ Left $ Undefined w
        Right _ -> return $ Left $ TypeError (show funcargs) "quoted list"
eval env (List (Word fun : args)) = do 
    bindings <- readIORef env
    let funcDef =  M.lookup fun bindings
    case funcDef of
        Nothing -> return $ Left $ Undefined (show fun)
        Just f -> evaluateArgs env f args
eval env val@(List [x]) = return $ Left $ NotFunc $ show x
eval env val@(Integer i) = return $ Right val
eval env val@(Boolean b) = return $ Right val
eval env val@(String s) = return $ Right val

evaluateArgs :: Environment -> LispVal -> [LispVal] -> IO EvalResult
evaluateArgs env func args = do
    argList <- mapM (eval env) args
    if (length . rights) argList == length args then
        apply env func $ rights argList -- if all args evaluate, pass to apply
    else
        (return . Left . head . lefts) argList -- otherwise, return error


apply :: Environment -> LispVal -> [LispVal] -> IO EvalResult
apply env func argVals =
    case func of
        DefaultFunc f -> return $ f argVals
        Function{args = args, body = body, closure = closure} ->
            if length args /= length argVals then
                return $ Left $ NumArgs (show func) (length args) (length argVals)
            else do
                let params = zip args argVals
                outerScope <- readIORef env
                innerScope <- newIORef $ M.union (M.fromList params) outerScope
                eval innerScope body
        _ -> return $ Left $ NotFunc (show func)
