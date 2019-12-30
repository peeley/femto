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

instance Eq LispVal where
    String x == String y = x == y
    Integer x == Integer y = x == y
    Word x == Word y = x == y
    Boolean x == Boolean y = x == y
    List x == List y = x == y
    DefaultFunc x == DefaultFunc y = False 
    -- this comparison defaulting to False shouldn't cause any problems
    -- for Data.Map, since new DefaultFuncs will never be added to the
    -- environment at runtime so no comparison needed
    x@Function{} == y@Function{} = x == y
    x == y = False

type Bindings = M.Map String LispVal 
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
