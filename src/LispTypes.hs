module LispTypes where

import Data.Map.Strict as M
import Data.IORef

data LispVal = String String
             | Integer Int 
             | Word String 
             | Boolean Bool 
             | List [LispVal]
             | DefaultFunc ([LispVal] -> EvalResult)
             | Function { args :: [String],
                          body :: LispVal,
                          closure :: Bindings }
               --deriving (Eq, Ord)


-- type Bindings = M.Map String LispVal #TODO: derive Eq, Ord for LispVal so
-- Bindings can be map, allows O(1) lookup for var/func definitions
type Bindings = [(String, LispVal)]
type Environment = IORef Bindings
type EvalResult = Either Error LispVal

instance Show LispVal where
    show (String s) = "\"" ++ s ++ "\""
    show (Integer i) = show i
    show (Word w) = w
    show (Boolean b) = if b then "#t" else "#f"
    show (List l) = "(" ++ (init . tail . show) l ++ ")"

data Error = TypeError String String | NumArgs String Int Int |
                    Undefined String | NotFunc String | ParseErr String |
                    Misc String deriving Eq


instance Show Error where
    show (TypeError varName expType) = "Error: " ++ varName ++ " is not of\
                                        \ expected type " ++ expType
    show (NumArgs func exp rec) = "Error: procedure " ++ func ++ " expected "
                                ++ show exp ++ " args, received " ++ show rec
    show (Undefined name) = "Error: symbol " ++ name ++ " is undefined."
    show (NotFunc name) = "Error: symbol " ++ name ++ " is not a procedure."
    show (ParseErr expr) = "Error: unable to parse " ++ expr
    show (Misc etc) = "Error: " ++ etc
