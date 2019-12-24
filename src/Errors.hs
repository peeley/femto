module Errors where

import Parser (LispVal)

data Error = TypeError String String | NumArgs String Int Int |
                    Undefined String | NotFunc String | ParseErr String |
                    Misc String

type EvalResult = Either Error LispVal

instance Show Error where
    show (TypeError varName expType) = "Error: " ++ varName ++ " expects " ++ 
                                        expType
    show (NumArgs func exp rec) = "Error: procedure " ++ func ++ " expected "
                                ++ show exp ++ " args, received " ++ show rec
    show (Undefined name) = "Error: symbol " ++ name ++ " is undefined."
    show (NotFunc name) = "Error: symbol " ++ name ++ " is not a procedure."
    show (ParseErr loc) = "Error: parser error at " ++ loc
    show (Misc etc) = "Error: " ++ etc
