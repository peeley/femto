module Eval where

import Parser
import Data.IORef
import qualified Data.Map.Strict as M

--type Runtime = IORef Environment -- for keeping track of all words
type Environment = M.Map String ([LispVal] -> LispVal) -- map words to values

eval :: Environment -> LispVal -> LispVal
eval _ (List [Word "quote", val]) = val
eval env (List (Word fun : args)) = apply env fun $ map (eval env) args
eval _ val = val

apply :: Environment -> String -> [LispVal] -> LispVal
apply env fun args = case M.lookup fun env of
                        Just f -> f args
                        Nothing -> error "Word not found."
