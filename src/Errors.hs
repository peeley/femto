module Errors where

import Parser (LispVal)

data Error = TypeError String String | NumArgs String Int Int |
                    Undefined String | NotFunc String | ParseErr String |
                    Misc String deriving Eq

type EvalResult = Either Error LispVal

instance Show Error where
    show (TypeError varName expType) = "Error: " ++ varName ++ " is not of\
                                        \ expected type " ++ expType
    show (NumArgs func exp rec) = "Error: procedure " ++ func ++ " expected "
                                ++ show exp ++ " args, received " ++ show rec
    show (Undefined name) = "Error: symbol " ++ name ++ " is undefined."
    show (NotFunc name) = "Error: symbol " ++ name ++ " is not a procedure."
    show (ParseErr expr) = "Error: unable to parse " ++ expr
    show (Misc etc) = "Error: " ++ etc
